{ PHANTOM.PAS -- revised from version in UNDOCUMENTED DOS, Chapter 4. 
  In particular, note use of the INT 2Fh AX=1208h and AX=120Ch functions
  in dec_SFT() and set_Owner(). This version works properly in DOS 5. }

{$A-,B-,D+,L+,E-,F-,I-,N-,O-,R-,S-,V-}
{$M 2048,128,1000}

program phantom_drive;
uses
    dos, crt;

type
    sig_rec = record
        signature : string[7];
        psp : word;
        drive_no : byte;
    end;

const
    cds_id_size = 10;
    cds_id = 'Phantom. :\';
    our : sig_rec =
        (   signature : 'PHANTOM'; psp : 0; drive_no : 0);
    vollab : string[13] = 'AN ILLUS.ION'#0; { Our Volume label }
    maxfilesize = 32767;                     { for our 1 file }

    isr_code_max = 102;                     { offset of last byte }
                                            { in our ISR macine code }

type
    strptr = ^string;
    cdsidarr = array[1..cds_id_size] of char;
    cdsidptr = ^cdsidarr;

{ FindFirst/Next data block - ALL DOS VERSIONS }
    sdb_ptr = ^sdb_rec;
    sdb_rec = record
        drv_lett : byte;
        srch_tmpl : array[0..10] of char;
        srch_attr : byte;
        dir_entry : word;
        par_clstr : word;
        f1 : array[1..4] of byte;
    end;

{ DOS System File Table entry - ALL DOS VERSIONS }
    sft_ptr = ^sft_rec;
    sft_rec = record
        handle_cnt,
        open_mode : word;
        attr_byte : byte;
        dev_info : word;
        devdrv_ptr : pointer;
        start_clstr,        { we don't need to touch this }
        f_time,
        f_size,
        f_pos : longint;
        rel_lastclstr,      { we don't need to touch this }
        abs_lastclstr,      { we don't need to touch this }
        dir_sector : word;  { we don't need to touch this }
        dir_entryno : byte; { we don't need to touch this }
        fcb_fn : array[0..10] of char;
    end;

{ DOS Current directory structure - DOS VERSION 3.xx }
    cds3_rec = record
        curr_path : array[0..66] of char;
        flags : word;
        f1 : array[1..10] of byte;  { we don't need to touch this }
        root_ofs : word;
    end;

{ DOS Current directory structure - DOS VERSION 4.xx }
    cds4_rec = record
        curr_path : array[0..66] of char;
        flags : word;
        f1 : array[1..10] of byte;  { we don't need to touch this }
        root_ofs : word;
        f2 : array[1..7] of byte;   { we don't need to touch this }
    end;

{ DOS Directory entry for 'found' file - ALL DOS VERSIONS }
    dir_ptr = ^dir_rec;
    dir_rec = record
        fname : array[0..10] of char;
        fattr : byte;
        f1 : array[1..10] of byte;
        time_lstupd,
        date_lstupd,
        start_clstr : word;         { we don't need to touch this }
        fsiz : longint;
    end;

{ Swappable DOS Area - DOS VERSION 3.xx }
    sda3_rec = record
         f0 : array[1..12] of byte;
         curr_dta : pointer;
         f1 : array[1..30] of byte;
         dd,
         mm : byte;
         yy_1980 : word;
         f2 : array[1..96] of byte;
         fn1,
         fn2 : array[0..127] of char;
         sdb : sdb_rec;
         found_file : dir_rec;
         drive_cdscopy : cds3_rec;
         fcb_fn1 : array[0..10] of char;
         f3 : byte;
         fcb_fn2 : array[0..10] of char;
         f4 : array[1..11] of byte;
         srch_attr : byte;
         open_mode : byte;
         f5 : array[1..48] of byte;
         drive_cdsptr : pointer;
         f6 : array[1..12] of byte;
         fn1_csofs,
         fn2_csofs : word;
         f7 : array[1..56] of byte;
         ren_srcfile : sdb_rec;
         ren_file : dir_rec;
    end;

{ Swappable DOS Area - DOS VERSION 4.xx }
    sda4_ptr = ^sda4_rec;
    sda4_rec = record
         f0 : array[1..12] of byte;
         curr_dta : pointer;
         f1 : array[1..32] of byte;
         dd,
         mm : byte;
         yy_1980 : word;
         f2 : array[1..106] of byte;
         fn1,
         fn2 : array[0..127] of char;
         sdb : sdb_rec;
         found_file : dir_rec;
         drive_cdscopy : cds4_rec;
         fcb_fn1 : array[0..10] of char;
         f3 : byte;
         fcb_fn2 : array[0..10] of char;
         f4 : array[1..11] of byte;
         srch_attr : byte;
         open_mode : byte;
         f5 : array[1..51] of byte;
         drive_cdsptr : pointer;
         f6 : array[1..12] of byte;
         fn1_csofs,
         fn2_csofs : word;
         f7 : array[1..71] of byte;
         spop_act,
         spop_attr,
         spop_mode : word;
         f8 : array[1..29] of byte;
         ren_srcfile : sdb_rec;
         ren_file : dir_rec;
    end;

{ DOS List of lists structure - DOS VERSIONS 3.1 thru 4 }
    lol_rec = record
        f1 : array[1..22] of byte;
        cds : pointer;
        f2 : array[1..7] of byte;
        last_drive : byte;
    end;

{ This serves as a list of the function types that we support }
    fxn_type = (_inquiry, _rd, _md, _cd, _close, _commit, _read,
                _write, _lock, _unlock, _space, _setattr, _getattr, 
                _rename, _delete, _open, _create, _ffirst, _fnext, 
                _seek, _specopen, _unsupported);

{ A de rigeur structure for manipulators of pointers }
    os = record o,s:word; end;

    fcbfnbuf = array[0..12] of char;
    fcbfnptr = ^fcbfnbuf;

    ascbuf = array[0..127] of char;
    ascptr = ^ascbuf;

{ This defines a pointer to our primary Int 2Fh ISR structure }
    isrptr = ^isr_rec;

{ A structure to contain all register values. The TP DOS registers 
    type is insufficient }
    regset = record 
        bp,es,ds,di,si,dx,cx,bx,ax,ss,sp,cs,ip,flags:word; end;

{ Our Int 2F ISR structure }
    isr_code_buffer = array[0..isr_code_max] of byte;
    isr_rec = record
        ic:isr_code_buffer;  { Contains our macine code ISR stub code }
        save_ss,             { Stores SS on entry before stack switch }
        save_sp,             { Stores SP on entry before stack switch }
        real_fl,             { Stores flags as they were on entry }
        save_fl,             { Stores flags from the stack }
        save_cs,             { Stores return CS from the stack }
        save_ip : word;      { Stores return IP from the stack }
        our_drive : boolean; { For ISR to either chain on or return }
    end;

    strfn = string[12];

const
 { all the calls we need to support are in the range 0..33 }
    fxn_map_max = $2e;
    fxn_map : array[0..fxn_map_max] of fxn_type =
                (_inquiry, _rd, _unsupported, _md, _unsupported,
                _cd, _close, _commit, _read, _write,
                _lock, _unlock, _space, _unsupported, _setattr, 
                _getattr, _unsupported, _rename, _unsupported,
                _delete, _unsupported, _unsupported, _open, _create, 
                _unsupported, _unsupported, _unsupported, _ffirst, _fnext,
                _unsupported, _unsupported, _unsupported, _unsupported,
                _seek, _unsupported, _unsupported, _unsupported, 
                _unsupported, _unsupported, _unsupported, _unsupported, 
                _unsupported, _unsupported, _unsupported, _unsupported, 
                _unsupported, _specopen
                );

{ The following are offsets into the ISR stub code where run time 
  values must be fixed in }
    prev_hndlr  = 99;
    redir_entry = 49;
    our_sp_ofs  = 45;
    our_ss_ofs  = 40;

{ The following offsets are known at compile time and are directly 
  referenced in the ISR stub code }
    save_ss_ofs = isr_code_max+1;
    save_sp_ofs = isr_code_max+3;
    save_rf_ofs = isr_code_max+5;
    save_fl_ofs = isr_code_max+7;
    save_cs_ofs = isr_code_max+9;
    save_ip_ofs = isr_code_max+11;
    our_drv_ofs = isr_code_max+13;

{ Our ISR stub code is defined as a constant array of bytes which 
  actually contains machine code as commented on the right }
    isr_code : isr_code_buffer = { entry: }
    (       $90,                { nop OR int 3          ; for debugging }
            $9c,                { pushf                 ; save flags    }
        $80,$fc,$11,            { cmp   ah,11h          ; our fxn?      }
        $75,$5a,                { jne   not_ours        ; bypass        }
    $2e,$8f,$06, save_rf_ofs, 0,{ pop   cs:real_fl      ; store act flgs}
    $2e,$8f,$06, save_ip_ofs, 0,{ pop   cs:save_ip      ; store cs:ip   }
    $2e,$8f,$06, save_cs_ofs, 0,{ pop   cs:save_cs      ; and flags     }
    $2e,$8f,$06, save_fl_ofs, 0,{ pop   cs:save_fl      ; from stack    }

    $2e,$89,$26, save_sp_ofs, 0,{ mov   cs:save_sp,sp   ; save stack    }
        $8c,$d4,                { mov   sp,ss                           }
    $2e,$89,$26, save_ss_ofs, 0,{ mov   cs:save_ss,sp                   }

        $bc,     0,0,           { mov   sp,SSEG         ; set our stack }
        $8e,$d4,                { mov   ss,sp                           }
        $bc,     0,0,           { mov   sp,SPTR                         }

        $9c,                    { pushf                 ; call our      }
        $9a,     0,0,0,0,       { call  redir           ; intr proc.    }

    $2e,$8b,$26, save_ss_ofs, 0,{ mov   sp,cs:save_ss   ; put back      }
        $8e,$d4,                { mov   ss,sp           ; caller's stack}
    $2e,$8b,$26, save_sp_ofs, 0,{ mov   sp,cs:save_sp                   }

    $2e,$ff,$36, save_fl_ofs, 0,{ push  cs:save_fl      ; restore       }
    $2e,$ff,$36, save_cs_ofs, 0,{ push  cs:save_cs      ; restore       }
    $2e,$ff,$36, save_ip_ofs, 0,{ push  cs:save_ip      ; return addr.  }
    $2e,$ff,$36, save_rf_ofs, 0,{ push  cs:real_fl      ; save act flgs }

    $2e,$80,$3e, our_drv_ofs,0,0,{ cmp cs:our_drive,0; not our drive?}
        $74,$04,                { je    not_ours        ; no, jump      }
        $9d,                    { popf                  ; yes, restore  }
        $ca,$02,$00,            { retf  2               ; & return flags}
                            { not_ours: }
        $9d,                    { popf                  ; restore flags }
        $ea,    0,0,0,0         { jmp   far prev_hndlr  ; pass the buck }
        );

var
{ The instance of our Int 2F ISR }
    isr : isrptr;

{ variables relating to the one allowable file.. }
    file_name : fcbfnbuf;
    file_buffer : array[0..maxfilesize] of byte;
{    file_opens, }
    file_date,
    file_time : word;
    file_attr : byte;
    file_size : longint;

{ Our full directory structure }
    max_path : ascbuf;

{ Global stuff }
    our_sp : word;          { SP to switch to on entry }
    dos_major,              { Major DOS vers }
    dos_minor,              { Minor DOS vers }
    drive_no : byte;        { A: is 1, B: is 2, etc. }
    strbuf : string;        { General purpose pascal string buffer }
    a1,                     { Pointer to an ASCIIZ string }
    a2 : ascptr;            { Pointer to an ASCIIZ string }
    drive : string[3];      { Command line parameter area }
    fxn : fxn_type;         { Record of function in progress }
    r : regset;             { Global save area for all caller's regs }
    temp_name : fcbfnbuf;   { General purpose ASCIIZ filename buffer }
    iroot,                  { Index to root directory in max_path }
    icur,                   { Index to current directory in max_path }
    lmax,                   { Length of max_path }
    ifile : byte;           { Index to directory in max_path with file }
    ver : word;             { full DOS version }
    sda : pointer;          { pointer to the Swappable Dos Area }
    lol : pointer;          { pointer to the DOS list of lists struct }

const h:array[0..15] of char = '0123456789abcdef';
type str4 = string[4];
function hex(inp:word):str4;
begin
    hex[0]:=#4;
    hex[1]:=h[inp shr 12];
    hex[2]:=h[(inp shr 8) and $f];
    hex[3]:=h[(inp shr 4) and $f];
    hex[4]:=h[inp and $f];
end;

{ Fail PHANTOM, print message, exit to DOS }
procedure failprog(msg:string);
begin
    writeln(msg);
    Halt(1);
end;

{ Get DOS version, address of Swappable DOS Area, and address of 
  DOS List of lists. We only run on versions of DOS >= 3.10, so
  fail otherwise }
procedure get_dos_vars;
var r : registers;
begin
    ver:=dosversion;
    dos_major:=lo(ver);
    dos_minor:=hi(ver);
    if (dos_major<3) or ((dos_major=3) and (dos_minor<10)) then
        failprog('DOS Version must be 3.10 or greater');
    with r do
        begin
            ax:=$5d06; msdos(r); sda:=ptr(ds,si);   { Get SDA pointer }
            ax:=$5200; msdos(r); lol:=ptr(es,bx);   { Get LoL pointer }
        end;
end;

{ Fail the current redirector call with the supplied error number, i.e.
  set the carry flag in the returned flags, and set ax=error code }
procedure fail(err:word);
begin
    r.flags:=r.flags or fcarry;
    r.ax:=err;
end;


{ Convert an 11 byte fcb style filename to ASCIIZ name.ext format }
procedure fnfmfcbnm(var ss; var p:ascptr);
var i,j:byte; s:ascbuf absolute ss;
    dot : boolean;
begin
    p:=@temp_name;
    i:=0;
    while (i<8) and (s[i]<>' ') do inc(i);
    move(s,p^,i);
    j:=8;
    while (j<11) and (s[j]<>' ') do inc(j);
    move(s,p^[succ(i)],j-8);
    if j<>8 then begin p^[i]:='.'; p^[j]:=#0; end
    else p^[i]:=#0;
end;    

{ The opposite of the above, convert an ASCIIZ name.ext filename 
  into an 11 byte fcb style filename }
procedure cnvt2fcb(var ss; var pp);
var i,j:byte;
    s:ascbuf absolute ss;
    p:ascbuf absolute pp;
begin
    i:=0; j:=0;
    fillchar(p,11,' ');
    while s[i]<>#0 do
        begin
            if s[i]='.' then j:=7 else p[j]:=s[i];
            inc(i);
            inc(j);
        end;
end;    

{ Get the length of an ASCIIZ string }
function asclen(var a:ascbuf):word;
var i:word;
begin i:=0; while (i<65535) and (a[i]<>#0) do inc(i); asclen:=i; end;

{ Translate a maximum of strlim bytes of an ASCIIZ string to a Pascal string }
procedure ascii2string(src, dst : pointer; strlim : byte);
var i:integer;
begin
    byte(dst^):=strlim;
    move(src^,pointer(succ(longint(dst)))^,strlim);
    i:=pos(#0,string(dst^));
    if i<>0 then byte(dst^):=pred(i);
end;

{ Set up global a1 to point to the appropriate source for the file
  or directory name parameter for this call }
procedure set_fn1;
begin
    case fxn of
{ For these calls, a fully qualified file/directory name is given in the
  SDA first filename field. This field, incidentally, can also be referenced
  indirectly through the SDA first filename offset field into DOS's CS. }
        _rd .. _cd, _setattr .. _create, _ffirst, _specopen :
            if dos_major=3 then
                a1:=@sda3_rec(sda^).fn1
            else
                a1:=@sda4_rec(sda^).fn1;

{ These do not need a filename. The following is valid-ish... }
        _close .. _write, _seek : a1:=@sft_rec(ptr(r.es,r.di)^).fcb_fn;

{ For findnext, an fcb style filename template is available within the
  SDA search data block field }
        _fnext :
            if dos_major=3 then
                a1:=@sda3_rec(sda^).sdb.srch_tmpl
            else
                a1:=@sda4_rec(sda^).sdb.srch_tmpl;
    end;
end;

{ Back up a directory level, ie go back to the previous \ in a path string }
function back_1(var path:ascbuf; var i:byte):boolean;
begin
    if i=iroot then begin back_1:=false; exit; end;
    repeat dec(i) until (i=iroot) or (path[i]='\');
    back_1:=true;
end;

{ Check that the qualified pathname that is in a1 matches our full
  directory structure to length lsrc. If not, fail with 'Path not found' }
function process_path(a1 : ascptr; lsrc : byte):boolean;
var isrc : byte;
begin
    process_path:=false;
    isrc:=0; 
    for isrc:=0 to pred(lsrc) do
        if (isrc>lmax) or
            (a1^[isrc]<>max_path[isrc]) then
                begin fail(3); exit; end;
    inc(isrc);
    if max_path[isrc]<>'\' then fail(3)
    else process_path:=true;
end;

function the_time:longint; inline($b8/$0d/$12/$cd/$2f);

{ Change Directory - subfunction 05h }
procedure cd;
var lsrc : byte;
begin
    lsrc:=asclen(a1^);
    if lsrc=succ(iroot) then dec(lsrc); { Special case for root }
    if not process_path(a1,lsrc) then exit;
    if dos_major=3 then             { Copy in the new path into the CDS }
        move(max_path,cds3_rec(sda3_rec(sda^).drive_cdsptr^).curr_path,lsrc)
    else
        move(max_path,cds4_rec(sda4_rec(sda^).drive_cdsptr^).curr_path,lsrc);
    icur:=lsrc;
end;

{ Remove Directory - subfunction 01h }
procedure rd;
var lsrc : byte;
begin
    lsrc:=asclen(a1^);
    if not process_path(a1,lsrc) then exit;
    if lsrc=icur then begin fail(5); exit; end;
    if lsrc=ifile then begin fail(5); exit; end;
    if lsrc<>lmax then begin fail(5); exit; end;
    if not back_1(max_path,lmax) then begin fail(3); exit; end;
    max_path[succ(lmax)]:=#0;
end;

{ Make Directory - subfunction 03h }
procedure md;
var lsrc, isrc : byte;
begin
    lsrc:=asclen(a1^);
    isrc:=lsrc;
    if not back_1(a1^,isrc) then begin fail(5); exit; end;
    if not process_path(a1,isrc) then exit;
    if isrc<>lmax then begin fail(5); exit; end;
    move(a1^,max_path,lsrc);
    max_path[lsrc]:='\';
    max_path[succ(lsrc)]:=#0;
    lmax:=lsrc;
end;

{ pop di   push cs   mov ax, 1208h   int 2fh }
function dec_SFT(es, di: word):word; inline($5f/$0e/$b8/$08/$12/$cd/$2f);

{ pop di   push cs   mov ax, 120ch   int 2fh }
procedure set_Owner(es, di: word); inline($5f/$0e/$b8/$0c/$12/$cd/$2f);

{ Close File - subfunction 06h }
procedure clsfil;
begin
{ Clear down supplied SFT entry for file }
    with sft_rec(ptr(r.es,r.di)^) do
        begin
            if dec_SFT(r.es,r.di)=1 then
                begin
                    handle_cnt:=0;
                    dir_sector:=0; { ??? MSCDEX does it.. }
                    devdrv_ptr:=nil; { ??? MSCDEX does it.. }
                end;
            if boolean(open_mode and 3) and
               not boolean(dev_info and $40) then
                                { if new or updated file... }
                    if f_time=0 then file_time:=the_time
                    else file_time:=f_time;
        end;
end;

{ Commit File - subfunction 07h }
procedure cmmtfil;
begin
{ We support this but don't do anything... }
end;

{ Read from File - subfunction 08h }
procedure readfil;
begin

{ Fill the user's buffer (the DTA) from our internal; file buffer, 
  and update the suplied SFT for the file }
    with sft_rec(ptr(r.es,r.di)^) do
        begin
            { if (f_pos+r.cx)>f_size then r.cx:=f_size-f_pos; }
            if f_pos >= f_size then r.cx := 0
            else if (f_pos + r.cx) > f_size then r.cx := f_size - f_pos;
            if dos_major=3 then
                move(file_buffer[f_pos],sda3_rec(sda^).curr_dta^,r.cx)
            else
                move(file_buffer[f_pos],sda4_rec(sda^).curr_dta^,r.cx);
            inc(f_pos,r.cx);
        end;
end;

{ Write to File - subfunction 09h }
procedure writfil;
begin

{ Update our internal file buffer from the user buffer (the DTA) and 
  update the supplied SFT entry for the file }
    with sft_rec(ptr(r.es,r.di)^) do
        begin
            if boolean(file_attr and readonly) then
                begin fail(5); exit; end; 
            if (f_pos+r.cx)>maxfilesize then r.cx:=maxfilesize-f_pos;
            if dos_major=3 then
                move(sda3_rec(sda^).curr_dta^,file_buffer[f_pos],r.cx)
            else
                move(sda4_rec(sda^).curr_dta^,file_buffer[f_pos],r.cx);
            inc(f_pos,r.cx);
            if f_pos>file_size then file_size:=f_pos;
            f_size:=file_size;
            dev_info:=dev_info and (not $40);
        end;
end;

{ Get Disk Space - subfunction 0Ch }
procedure dskspc;
begin
{ Our 'disk' has 1 cluster containing 1 sector of maxfilesize bytes, and ... }
    r.ax:=1; 
    r.bx:=1;
    r.cx:=succ(maxfilesize);
{ ... its either all available or none! }
    r.dx:=ord(ifile=0);
end;

{ Set File Attributes - subfunction 0Eh }
procedure setfatt;
var lsrc, isrc : byte;
begin
    lsrc:=asclen(a1^);
    isrc:=lsrc;
    if not back_1(a1^,isrc) then begin fail(2); exit; end;
    if not process_path(a1,isrc) then exit;
    if isrc<>ifile then begin fail(2); exit; end;
    inc(isrc);
    fillchar(temp_name,13,#0);
    move(a1^[isrc],temp_name,lsrc-isrc);
    if temp_name<>file_name then begin fail(2); exit; end;
{    if file_opens>0 then fail(5) 
    else }  file_attr:=byte(ptr(r.ss,r.sp)^);
end;

{ Get File Attributes - subfunction 0Fh }
procedure getfatt;
var lsrc, isrc : byte;
begin
    lsrc:=asclen(a1^);
    isrc:=lsrc;
    if not back_1(a1^,isrc) then begin fail(2); exit; end;
    if not process_path(a1,isrc) then exit;
    if isrc<>ifile then begin fail(2); exit; end;
    inc(isrc);
    fillchar(temp_name,13,#0);
    move(a1^[isrc],temp_name,lsrc-isrc);
    if temp_name<>file_name then begin fail(2); exit; end;
{    if file_opens>0 then begin fail(5); exit; end; }
    r.ax:=file_attr;
end;

{ Rename File - subfunction 11h }
procedure renfil;
var lsrc, isrc, isav, i : byte;
    dot:boolean;
begin
    if dos_major=3 then
        a2:=ptr(r.ss,sda3_rec(sda^).fn2_csofs)
    else
        a2:=ptr(r.ss,sda4_rec(sda^).fn2_csofs);
    lsrc:=asclen(a1^);
    isrc:=lsrc;
    if not back_1(a1^,isrc) then begin fail(3); exit; end;
    if not process_path(a1,isrc) then exit;
    if isrc<>ifile then begin fail(2); exit; end;
    inc(isrc);
    fillchar(temp_name,13,#0);
    move(a1^[isrc],temp_name,lsrc-isrc);
{ Check that the current filename matches ours }
    if temp_name<>file_name then begin fail(2); exit; end;
    if boolean(file_attr and $7) then begin fail(5); exit; end;
{    if file_opens>0 then begin fail(5); exit; end; }
    lsrc:=asclen(a2^);
    isrc:=lsrc;
    if not back_1(a2^,isrc) then begin fail(3); exit; end;
    if not process_path(a2,isrc) then exit;
    ifile:=isrc;
    inc(isrc);
{ Put in the new file name }
    fillchar(file_name,13,#0);
    move(a2^[isrc],file_name,lsrc-isrc);
end;

{ This procedure does a wildcard match from the mask onto the target, and,
  if a hit, updates the search data block and found file areas supplied } 
function match(var m, t; var s : sdb_rec; var d : dir_rec;
                d_e, p_c : word; s_a : byte) : boolean;
var i, j : byte;
    mask : ascbuf absolute m;
    tgt : ascbuf absolute t;
begin
    i:=0; j:=0;
    if tgt[0] in ['\',#0] then begin match:=false; exit; end;
    while i<11 do
        case mask[i] of
            '?' :   if tgt[j] in [#0,'\','.'] then
                        if (i=8) and (tgt[j]='.') then inc(j) else inc(i)
                    else
                        begin inc(i); inc(j); end;
            ' ' :   if tgt[j] in ['.','\',#0] then inc(i)
                    else begin match:=false; exit; end;
            else    if (i=8) and (tgt[j]='.') then inc(j)
                    else
                    if tgt[j]=mask[i] then begin inc(i); inc(j); end
                    else begin match:=false; exit; end;
        end;
    if not (tgt[j] in ['\',#0]) then begin match:=false; exit; end;
    with s do
        begin
            move(mask,srch_tmpl,11);
            dir_entry:=d_e;
            srch_attr:=s_a;
            par_clstr:=p_c;
            drv_lett:=drive_no or $80;
        end;
    with d do
        begin
            i:=0; j:=0;
            fillchar(fname,11,' ');
            while not (tgt[i] in [#0,'\']) do
                if tgt[i] = '.' then begin j:=8; inc(i); end
                else begin fname[j]:=tgt[i]; inc(i); inc(j); end;
            case d_e of
                0 : fattr:=$08;
                1 : fattr:=$10;
                2 : fattr:=file_attr;
            end;
            time_lstupd:=file_time;
            date_lstupd:=file_date;
            case d_e of
                0, 1 : fsiz:=0;
                2 : fsiz:=file_size;
            end;
        end;
    match:=true;
end;

{ Delete File - subfunction 13h }
procedure delfil;
var isrc, lsrc : byte;
    sdb:sdb_rec;    { These are dummies for the match procedure to hit }
    der:dir_rec;
begin
    lsrc:=asclen(a1^);
    isrc:=lsrc;
    if not back_1(a1^,isrc) then begin fail(3); exit; end;
    if not process_path(a1,isrc) then exit;
    if isrc<>ifile then begin fail(2); exit; end;

    inc(os(a1).o,succ(isrc));
    cnvt2fcb(a1^,temp_name);
    if ((file_attr and $1f)>0) then begin fail(5); exit; end;
    if not match(temp_name,file_name,sdb,der,0,0,0) then
        begin fail(2); exit; end;
    { if file_opens=0 then } ifile:=0 { else fail(5) } ;
end;

{ Open Existing File - subfunction 16h }
procedure opnfil;
var isrc, lsrc : byte;
begin
    lsrc:=asclen(a1^);
    isrc:=lsrc;
    if not back_1(a1^,isrc) then begin fail(3); exit; end;
    if not process_path(a1,isrc) then exit;
    if isrc<>ifile then begin fail(2); exit; end;
    inc(isrc);
    fillchar(temp_name,13,#0);
    move(a1^[isrc],temp_name,lsrc-isrc);
{ Check file names match }
    if temp_name<>file_name then begin fail(2); exit; end;

{ Initialize supplied SFT entry }
    with sft_rec(ptr(r.es,r.di)^) do
        begin
            file_attr:=byte(ptr(r.ss,r.sp)^);
            if dos_major=3 then
                open_mode:=sda3_rec(sda^).open_mode and $7f
            else
                open_mode:=sda4_rec(sda^).open_mode and $7f;
            cnvt2fcb(temp_name,fcb_fn);
         {   inc(file_opens); }
            f_size:=file_size;
            f_time:=file_time;
            dev_info:=$8040 or drive_no; { Network drive, unwritten to }
            dir_sector:=0;
            dir_entryno:=0;
            attr_byte:=file_attr;
            f_pos:=0;
            devdrv_ptr:=nil;
            set_Owner(r.es,r.di);
        end;
end;

{ Truncate/Create File - subfunction 17h }
procedure creatfil;
var isrc, lsrc : byte;
begin
    lsrc:=asclen(a1^);
    isrc:=lsrc;
    if not back_1(a1^,isrc) then begin fail(3); exit; end;
    if not process_path(a1,isrc) then exit;

    if ifile=0 then 
        begin
{ Creating new file }
            ifile:=isrc;
            inc(isrc);
            if isrc=lsrc then begin fail(13); ifile:=0; exit; end;
            fillchar(file_name,13,#0);
            move(a1^[isrc],file_name,lsrc-isrc);
        end
    else

    if ifile=isrc then
        begin
{ Truncate existing file }
            inc(isrc);
            fillchar(temp_name,13,#0);
            move(a1^[isrc],temp_name,lsrc-isrc);
            if temp_name<>file_name then begin fail(2); exit; end;
            if boolean(file_attr and $7) then begin fail(5); exit; end;
          {  if file_opens>0 then begin fail(5); exit; end; }
        end
    else fail(82);  { This provokes a 'ran out of dir entries' error }

{ Initialize supplied SFT entry }
    with sft_rec(ptr(r.es,r.di)^) do
        begin
            file_attr:=byte(ptr(r.ss,r.sp)^); { File attr is top of stack }
            open_mode:=$01;     { assume an open mode, none is supplied.. }
            cnvt2fcb(file_name,fcb_fn);
           { inc(file_opens); }
            f_size:=0;
            f_pos:=0;
            file_size:=0;
            dev_info:=$8040 or drive_no; { Network drive, unwritten to }
            dir_sector:=0;
            dir_entryno:=0;
            f_time:=0;
            devdrv_ptr:=nil;
            attr_byte:=file_attr;
            set_Owner(r.es,r.di);
        end;
end;

{ Special Multi-Purpose Open File - subfunction 2Eh }
procedure spopnfil;
var isrc, lsrc : byte;
    action, mode, result : word;
begin
    lsrc:=asclen(a1^);
    isrc:=lsrc;
    if not back_1(a1^,isrc) then begin fail(3); exit; end;
    if not process_path(a1,isrc) then exit;
    mode:=sda4_rec(sda^).spop_mode and $7f;
    action:=sda4_rec(sda^).spop_act;
{ First, check if file must or must not exist }
    if ((((action and $f)=0) and (isrc<>0)) or
        (((action and $f0)=0) and (isrc=0))) then begin fail(5); exit; end;

    if ifile=0 then 
        begin
{ Creating new file }
            result:=2;
            ifile:=isrc;
            inc(isrc);
            if isrc=lsrc then begin fail(13); ifile:=0; exit; end;
            fillchar(file_name,13,#0);
            move(a1^[isrc],file_name,lsrc-isrc);
        end
    else

    if ifile=isrc then
        begin
{ Open/Truncate existing file }
            inc(isrc);
            fillchar(temp_name,13,#0);
            move(a1^[isrc],temp_name,lsrc-isrc);
            if temp_name<>file_name then begin fail(82); exit; end;
            if boolean(action and 2) then
                result:=3           { File existed, was replaced }
            else
                result:=1;          { File existed, was opened }
            if boolean(file_attr and $1) and
                ((result=3) or ((mode and 3)>0)) then
                begin fail(5); exit; end;   { It's a read only file }
            if (result=3) { and (file_opens>0) } then
                begin fail(5); exit; end;   { Truncating an open file }
        end
    else fail(5);

{ Initialize the supplied SFT entry }
    with sft_rec(ptr(r.es,r.di)^) do
        begin
            if result>1 then
                begin
                    file_attr:=byte(ptr(r.ss,r.sp)^); { Attr is top of stack }
                    f_size:=0;
                    file_size:=0;
                end;
            open_mode:=mode;
            cnvt2fcb(file_name,fcb_fn);
            { inc(file_opens); }
            f_pos:=0;
            f_time:=0;
            dev_info:=$8040 or drive_no; { Network drive, unwritten to }
            dir_sector:=0;
            dir_entryno:=0;
            devdrv_ptr:=nil;
            attr_byte:=file_attr;
            set_Owner(r.es,r.di);
        end;
end;

{ FindFirst - subfunction 1Bh }
procedure ffirst;
var isrc, lsrc : byte;
    sdb : sdb_ptr;
    der : dir_ptr;
    sa, fa : byte;
begin
    lsrc:=asclen(a1^);
    isrc:=lsrc;
    if not back_1(a1^,isrc) then begin fail(3); exit; end;
    if not process_path(a1,isrc) then exit;
    a2:=@max_path;
    if dos_major=3 then
        begin
            a1:=@sda3_rec(sda^).fcb_fn1;
            sdb:=@sda3_rec(sda^).sdb;
            der:=@sda3_rec(sda^).found_file;
            sa:=sda3_rec(sda^).srch_attr;
        end
    else
        begin
            a1:=@sda4_rec(sda^).fcb_fn1;
            sdb:=@sda4_rec(sda^).sdb;
            der:=@sda4_rec(sda^).found_file;
            sa:=sda4_rec(sda^).srch_attr;
        end;
    fa:=file_attr and $1e;
    inc(os(a2).o,succ(isrc));

{ First try and match volume label, if asked for }
    if ((sa=$08) or (boolean(sa and $08) and (isrc=iroot))) and
       match(a1^,vollab[1],sdb^,der^,0,isrc,sa) then exit;

{ Then try the one possible subdirectory, if asked for and if it exists }
    if boolean(sa and $10) and
       match(a1^,a2^,sdb^,der^,1,isrc,sa) then exit;

{ Finally try the one possible file, if asked for, if it exists, and if
  in this subdirectory }
    if (ifile=isrc) and 
       ((fa=0) or boolean(sa and fa)) and
       match(a1^,file_name,sdb^,der^,2,isrc,sa) then exit;

{ Otherwise report no more files }
    fail(18);
end;

{ FindFirst - subfunction 1Bh }
procedure fnext;
var fa : byte;
    sdb : sdb_ptr; der : dir_ptr;
begin
    if dos_major=3 then
        begin
            sdb:=@sda3_rec(sda^).sdb;
            der:=@sda3_rec(sda^).found_file;
        end
    else
        begin
            sdb:=@sda4_rec(sda^).sdb;
            der:=@sda4_rec(sda^).found_file;
        end;
    fa:=file_attr and $1e;
    inc(sdb^.dir_entry);
    case sdb^.dir_entry of
        1 : a2:=@max_path[succ(sdb^.par_clstr)];
        2 : a2:=@file_name;
        else begin fail(18); exit; end;
    end;

{ First try the one possible subdirectory, if it exists. FNext can never
  match a volume label }
    if (sdb^.dir_entry=1) and boolean(sdb^.srch_attr and $10) and
        match(a1^,a2^,sdb^,der^,
            sdb^.dir_entry,sdb^.par_clstr,sdb^.srch_attr) then exit;

{ Then try the one possible file, if exists, and if in this subdirectory }
    if sdb^.dir_entry=1 then
        begin a2:=@file_name; sdb^.dir_entry:=2; end;
    if (sdb^.dir_entry=2) and (ifile=sdb^.par_clstr) and
        ((fa=0) or boolean(sdb^.srch_attr and fa)) and
        match(a1^,a2^,sdb^,der^,
            sdb^.dir_entry,sdb^.par_clstr,sdb^.srch_attr) then exit;

{ Otherwise return no more files }
    fail(18);
end;

{ Seek From End Of File - subfunction 21h }
procedure skfmend;
var skamnt : longint;
begin
    skamnt:=(longint(r.cx)*65536)+r.dx;
    { if file_opens=0 then begin fail(5); exit; end; }

{ Update supplied SFT entry for file }
    with sft_rec(ptr(r.es,r.di)^) do
        begin
            f_pos:=f_size-skamnt;
            r.dx:=f_pos shr 16;
            r.ax:=f_pos and $ffff;
        end;
end;

function call_for_us(es,di:word):boolean;
var p:pointer;
begin
    if (fxn in [_close.._unlock,_seek]) then
        call_for_us:=(sft_rec(ptr(es,di)^).dev_info and $1f)=drive_no
    else
    if fxn=_inquiry then call_for_us:=true
    else
        begin
            if dos_major=3 then p:=sda3_rec(sda^).drive_cdsptr
            else p:=sda4_rec(sda^).drive_cdsptr;
            call_for_us:=cdsidptr(p)^=cdsidptr(@max_path)^;
        end;
end;

{ This is the main entry point for the redirector. The procedure is actually
  invoked from the Int 2F ISR stub via a PUSHF and a CALL FAR IMMEDIATE
  instruction to simulate an interrupt.  That way we have many of the
  registers on the stack and DS set up for us by the TP interrupt keyword.
  This procedure saves the registers into the regset variable, assesses if
  the call is for our drive, and if so, calls the appropriate routine. On
  exit, it restores the (possibly modified) register values. }
procedure redirector(_flags,_cs,_ip,_ax,_bx,_cx,_dx,_si,_di,_ds,_es,_bp:word);
    interrupt;
begin
    with r do
        begin
            isr^.our_drive:=false;
{ If we don't support the call, pretend we didn't see it...! }
            if lo(_ax)>fxn_map_max then exit
            else fxn:=fxn_map[lo(_ax)];
            if fxn=_unsupported then exit;
{ If the call isn't for our drive, jump out here... }
            if not call_for_us(_es,_di) then exit;
{ Set up our full copy of the registers }
            isr^.our_drive:=true;
            move(_bp,bp,18); ss:=isr^.save_ss; sp:=isr^.save_sp;
            cs:=isr^.save_cs; ip:=isr^.save_ip; flags:=isr^.real_fl;
            ax:=0; flags:=flags and not fcarry;
            set_fn1;
            case fxn of
                _inquiry    : r.ax:=$00ff;
                _rd         : rd;
                _md         : md;
                _cd         : cd;
                _close      : clsfil;
                _commit     : cmmtfil;
                _read       : readfil;
                _write      : writfil;
                _space      : dskspc;
                _setattr    : setfatt;
                _lock, _unlock : ; 
                _getattr    : getfatt;
                _rename     : renfil;
                _delete     : delfil;
                _open       : opnfil;
                _create     : creatfil;
                _specopen   : spopnfil;
                _ffirst     : ffirst;
                _fnext      : fnext;
                _seek       : skfmend;
            end;
{ Restore the registers, including any that we have modified.. }
            move(bp,_bp,18); isr^.save_ss:=ss; isr^.save_sp:=sp;
            isr^.save_cs:=cs; isr^.save_ip:=ip; isr^.real_fl:=flags;
        end;
end;

{ This procedure sets up our ISR stub as a structure on the heap. It
  also ensures that the structure is addressed from an offset of 0 so
  that the CS overridden offsets in the ISR code line up. Finally. it
  fixes in some values which are only available to us at run time,
  either because they are variable, or because of limitations of the
  language. }
procedure init_isr_code;
var p:pointer;
    i:pointer absolute isr;
begin
    getmem(isr,sizeof(isr_rec)+15);
    inc(os(isr).s,(os(isr).o+15) shr 4);
    isr^.ic:=isr_code;
    getintvec($2f,p);
    os(isr).o:=redir_entry; pointer(i^):=@redirector;
    os(isr).o:=our_ss_ofs; word(i^):=sseg;
    os(isr).o:=our_sp_ofs; word(i^):=our_sp;
    os(isr).o:=prev_hndlr; pointer(i^):=p;
    os(isr).o:=0;
end;

{ Do our initializations }
procedure init_vars;
    function installed_2f:byte;
        { mov ax,1100h   int 2fh }
        inline($b8/$00/$11/$cd/$2f);
begin
    if installed_2f=1 then
        failprog('Not OK to install a redirector...'); 
    drive_no:=byte(drive[1])-byte('@');
    our_sp:=sptr+$100;
    { file_opens:=0; }
{ Note that the assumption is that we lost 100h bytes of stack
  on entry to main }
{ Initialise and fix-up the master copy of the ISR code }
    init_isr_code;
    ifile:=0;
end;

{ This is where we do the initializations of the DOS structures
  that we need in order to fit the mould }
procedure set_path_entry;
var our_cds:pointer;
begin
    our_cds:=lol_rec(lol^).cds;
    if dos_major=3 then
        inc(os(our_cds).o,sizeof(cds3_rec)*pred(drive_no))
    else
        inc(os(our_cds).o,sizeof(cds4_rec)*pred(drive_no));
    if drive_no>lol_rec(lol^).last_drive then
        failprog('Drive letter higher than last drive...'); 

{ Edit the Current Directory Structure for our drive }
    with cds3_rec(our_cds^) do
        begin
            ascii2string(@curr_path,@strbuf,255);
            writeln('Curr path is ',strbuf);
            if (flags and $c000)<>0 then
                failprog('Drive already assigned.');
            flags:=flags or $c000;  { Network+Physical bits on ... }
            strbuf:=cds_id;
            strbuf[length(strbuf)-2]:=char(byte('@')+drive_no);
            move(strbuf[1],curr_path,byte(strbuf[0]));
            move(curr_path,max_path,byte(strbuf[0]));
            curr_path[byte(strbuf[0])]:=#0;
            max_path[byte(strbuf[0])]:=#0;
            root_ofs:=pred(length(strbuf));
            iroot:=root_ofs;
            lmax:=iroot;
        end;
end;

{ Use in place of Turbo's 'keep' procedure. It frees the environment
  and keeps the size of the TSR in memory smaller than 'keep' does }
procedure tsr;
var r:registers;
begin
    swapvectors;
    r.ax:=$4900;
    r.es:=memw[prefixseg:$2c];
    msdos(r);
    r.ax:=$3100;
    r.dx:=os(heapptr).s-prefixseg+1;
    msdos(r);
end;

procedure settle_down;
var p:pointer;
    i:integer;
    w:word;
begin
{ Plug ourselves into Int 2F }
    setintvec($2f,isr);
    writeln('Phantom drive installed as ',drive[1],':');
{ Find ourselves a free interrupt to call our own. Without it, future
  invocations of Phantom will not be able to unload us. }
    i:=$60;
    while (i<=$67) and (pointer(ptr(0,i shl 2)^)<>nil) do inc(i);
    if i=$68 then 
        begin
            writeln('No user intrs available. PHANTOM not unloadable..');
            tsr;
        end;
{ Have our new found interrupt point at the command line area of 
  our PSP. Complete our signature record, put it into the command line, 
  and go to sleep. }
    w:=$80;
    setintvec(i,ptr(prefixseg,w));
    our.psp:=prefixseg;
    our.drive_no:=drive_no;
    sig_rec(ptr(prefixseg,w)^):=our;
    tsr;
end;

{ Find the latest Phantom installed, unplug it from the Int 2F chain if
  possible, undo the dpb chain, make the CDS reflect an invalid drive,
  and free its memory.. }
procedure do_unload;
var i:integer; p, cds:pointer; w:word; r:registers;
begin
    i:=$67;
    while (i>=$60) and
      (sig_rec(pointer(ptr(0,i shl 2)^)^).signature<>our.signature) do
        dec(i);
    if i=$5f then 
        begin writeln(our.signature,' not found...'); halt; end;
    getintvec($2f,p);
    if os(p).o<>0 then 
        failprog('2F superceded...'); 
    os(p).o:=prev_hndlr;
    setintvec($2f,pointer(p^));
    getintvec(i,p);
    drive_no:=sig_rec(p^).drive_no;
    with r do
        begin
            ax:=$4900; es:=sig_rec(p^).psp;
            msdos(r);
            if boolean(flags and fcarry) then
                writeln('Could not free main memory...');
        end;
    setintvec(i,nil);
    cds:=lol_rec(lol^).cds;
    if dos_major=3 then
        inc(os(cds).o,sizeof(cds3_rec)*pred(drive_no))
    else
        inc(os(cds).o,sizeof(cds4_rec)*pred(drive_no));
    with cds3_rec(cds^) do flags:=flags and $3fff;
    writeln('Drive ',char(byte('@')+drive_no),': is now invalid.');
end;

begin { MAIN }
{ Check parameter count }
    if (paramcount<>1) then
        failprog('Usage is: PHANTOM drive-letter:'); 
    drive:=paramstr(1);
    drive[1]:=upcase(drive[1]);
{ If this is an unload request, go to it }
    if (drive='-u') or (drive='-U') then
        begin
            get_dos_vars;
            do_unload;
            halt;
        end;
{ Otherwise, check that it's a valid drive letter }
    if  (length(drive)>2) or
        not (drive[1] in ['A'..'Z']) or
        ((length(drive)=2) and (drive[2]<>':'))
            then failprog('Usage is: PHANTOM drive-letter:'); 
{ ... and set up shop }
    init_vars;
    get_dos_vars;
    set_path_entry;
    settle_down;
end.
