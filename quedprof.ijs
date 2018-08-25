NB.  create / cd to jkt locale
18!:4 <'jkt' NB. doing this explicitly because we are "pre-boot"
COCLASSPATH=: 'jkt';,'z'
NB.  coclass'jkt'
NB.
NB.=.=======================================================
NB. add local stuff here 

NB. Some debugging aids -
  debug=: 13!:0
  showsi=: 13!:1
  signal=: 13!:8
  resume=: 13!:4
NB. attempt to use debug to catch errors in this script
NB. unfortunately, it did't work for spelling errors ...  debug 1
NB. to be reset with debug 0  near the end of the script.

NB. Some paths for "portability"
  BELLCORE=: '/home/jkt/bellcore/'
NB.  version=: 3 : '(9!:14 ''''), (10{a.), ''Running in: '', UNAME, (10{a.), JVERSION, host ''sw_vers'''
  version=: 3 : 'JVERSION, (host ''sw_vers''), 2!:0 ''echo `java -version 2>&1`'''
  9!:37] 0 10000 150 150 NB. set longer output lines
NB. or will this work (above overwritten)
  Output=: 0 10000 150 150
  Output_z_=: 0 10000 150 150

NB. set line length and number of lines hnt
NB. was  9!:37 ]0 5555 0 555
NB. 9!:27'9!:37]0 5555 0 555'
NB. 9!:29]1

NB. Curious...
9!:11]10
9!:1]314159

NB. General Utilities -
 Note=: 3 :'''''[ 0 : 0'
 qscript=: 0!:10@<
 NB. fread=: 1!:1@<
 NB. fwrite=: 1!:2<
 NB. fappend=: 1!:3<
 NB. fsize=: 1!:4@<
 ir=: 1!:11
 timex=: 6!:2 , 7!:2@]
 spacex=: 7!:2
 ts=: 6!:0
 nl=: 4!:1
 ex=: 3 : '4!:55 <;._1 y'
 exit=: 2!:55
 listfiles=: 3 : 'box host ''ls '',y'
 echo=: 1!:2&2@]
 qprompt=: 3 : ' 1!:1 1: y (1!:2) 5'

 wec=: 13 : ',''&#'',"1 (0 1}. ": 1000 + a.i. ,.y),. '';''' NB. Web Encode Characters (useful for email obfuscation)

NB. Some useful variables...
    chars=: ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~'
   lchars=: 'abcdefghijklmnopqrstuvwxyz' ((a.i.'A')+i. 26) } a.
   uchars=: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ((a.i.'a')+i. 26) } a.
   TAB=: 9{a.    NB. Tab and other ASCII control characters.
   LF=: 10{a.    NB. Convenience name for Line Feed character.
   CR=: 13{a.    NB. Carriage Return (??)
   DL=: TAB      NB. Tab is delimiter character of choice... (also "|")
   NL=: LF       NB. Would be CR on Macintosh
   DOSNL=: CR,LF NB. Set NL to this for BDOS work...

   unbdos=: 13 : '((y~: 26{a.) *. -.DOSNL E. y)#y' NB. Remove CRs from DOSD...
   number=: 13 : '0 ". digits y'
   nuntable=: 13 : ' ;"_1> 0&". &.> y'  NB. Untables numbers 0s for empty or non-numeric
      NB. unfortunately, the above only works (correctly) for rank 2 y values...


NB. For some "pretty good random number seeds"
   pgseed=: 3 : '9!:1 {. _2 (3!:4) (4?4){ host ''dd if=/dev/random bs=4 count=1'''

   rnd=: 13 : 'x * <. 0.5 + y % x'
  
 NB. digits=: 13 : '(y e. ''0123456789'')#y'
   digits=: -.&(a.-.'0123456789') NB. Thank you Roger!
   ab=: #.^:_1

   pfmt=: 13 : '}:;(,.(($y)$1 0 0 1 0 0 1 0 0 0 1 0 0 0 )<;.1 y),.<'' '''
   pncompress=: 13 : 'pfmt digits y'
   txcompress=: 13 : '(":(|.-.*./\|.'' ''=y)#y)'  NB. Cleans up text.
   nob=: 13 : 'y-.'' '''  NB. Removes all blanks
   noquotes=: 13 : 'y -. ''"'''
NB. ntb=: 13 : '(-+/*./\'' ''=|. y) }. y' NB. Removes trailing blanks
    ntb=: 13 : '(>:(y~:'' '')i: 1){.y'      NB. Removes trailing blanks
    nbb=: 13 : ';(1, }.y E.x) <@ntb ;.1 x'   NB. Remove Blanks in x Before y  e.g. 'long text' rbb NL
    ndb=: 13 : '(+./'' ''~: y,: }.y,''*'') # y' NB. Removes multiple blanks
    nels=: 13 : '(+./LF ~: y,: 1|. y) # y'  NB. Removes empty lines
   ttcs=: 13 : '((#y)<.2+(y~:x)i: 1){.y'  NB. Trims trailing (delimiter) characters.
   rl=: 13 : '#;.1~1,2~:/\y'  NB. run lengths of a vector

   notr=: 3 : 0   NB. NOTRailing pattern x (default blank)
 ' ' notr y
:
 (>: 0 i:~ +./(-i.#x)|."0 1 x E. y){.y
)

   b64dc=: 3 : 0   NB. Base 64 decode (e.g. from email)
b64=. ((,65 97 +/ i.26),43 47,~ (48+/i.10)){a.
in=. y -. DOSNL,' '
  if. ((0 ~: 4 | #in) +. _2 > pad=. - +/ '='=._4{.in) do. '** Input Data Corrupt **' return. end.
pad}. a. {~ _8 #.\ ,#: 64| b64 i. in
)
   
   b64ec=: 3 : 0  NB. Base 64 encode (MIME standard) 
b64=. ((,65 97 +/ i.26),43 47,~ (48+/i.10)){a.
pad=. 3|3-3|#y
ntb ,_76 ,&NL\(pad#'='),~(_6 #.\(0#~2*pad),~,}:#:a.i.y,{:a.){b64
)

   slog=: 3 : (':'; 'x fappend~ NL,~ ('' - '' ,~ 8 7": 0 100 100 #. 2 3$ ts ''''), y') 

   where=: 3 : (':'; '   (;+./&.> y&E. &.>x)#i.$x')
   iin=: [: I. E.  NB. Index IN   -- 'abc' iin 'ababc'

   lcname=: 13 : 'nob (95 | chars i. y){32 }.lchars'
   ucname=: 13 : 'nob (95 | chars i. y){32 }.uchars'
   dkey=: [: -.&(a.-.'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789') ucase  
   ucase=: 13 : '(a. i. y){uchars'
   lcase=: 13 : '(a. i. y){lchars'

   hist=: 13 : '(~.y),:#/.~y'
   dist=: 13 : 'hist #/.~y'
   shist=: 13 : '(/: 0{ y) {"1 y'
   ahist=: 13 : '/:~ (<"0 a.i.~.y),. (<"0 ~.y),. <"0 #/.~y'
   bhist=: 13 : '/:~ (~. y),. <@":"0 #/.~y'
   sbhist=: 13 : 'y \: 0".>1{"1 y'

   box=: 13 : '<;._2 y'
   unbox=: 13 : '; ,&LF &.> y' 
   depunct=: 3 : ''' '' ((y e.a.-.'' abcdefghijklmnopqrstuvwxyz'')#i.#y)}y'

   wordfreq=: 3 : 0
  _ wordfreq y
: 
   words=. box ndb depunct lcase y, ' '
   uwords=. ~. words
   freq=. #/.~ words
 (x <.#uwords) $ (uwords,. <@":"0 freq)\:freq 
)

   table=: 3 : 0
  <;._2;._2 y
:
  <;._2 @ (,& x) ;. _2 y
)

   untable=: 3 : 0
  TAB untable y
 :
  ; ((,(i.1{$y),. 1{$y),1+1{$y){"1 y,"1 x ;NL
)

   fmtable=: 3 : 0
 ' ' fmtable y  NB. default "formatter" is a blank...
:
 ;"1 (,(i.1{$y),.1{$y){"1 y,.<x
)

   rqnls=: 3 : 0
 '~' rqnls y    NB. Remove Quoted NLs - preprocessor for tablecsv
:
 x ((#i.@#)(y e.NL)*. ~:/\y e.'"')}y
)

   tablecsv=: 3 : 0
    ',' tablecsv y
:
    x &cutl;._2 y
)
   cutl=: 3 : 0
:
 y=. y,x
 q=. y e.'"'
 qs=. ~:/\q
 }.^: ('"'&=@([:{.1:{.]))&.> (qs<y e.x) <;._2&((q<:qs)&#) y
)

   table2csv=: 3 : 0
NB. Set NL global to DOSLE for BDOS output
  DEL=. _1 44 _1{a.  NB. Marker for ","
  LNL=. 1|. NL,~_1 _1{a.  NB. Local New line...
  ob=. _1|. ; ((}:,(,. i. 1{$y),. 1{$y),1+1{$y){"1 y,"1 DEL;LNL
  ob=. (1+ob='"')#ob  NB. Double the "s
  '"' ((ob=_1{a.)#i.#ob) } ob  NB. Put "s in appropriate places
)


  dups=: 3 : 0
 y dups y
:
dm=. y e. (-.~:y)#y
(dm #y) </. dm#x
)

   d1=: 3 : 0
 '1' d1 y  NB. Default is to "drop 1" from beginning of phone numbers
:
 (x=. {.y)}.y
)

   today=: 3 : '<. 0 100 100#. 3{. ts '''''

   currentacs=: 3 : 0
 (today'' ) currentacs y
:
 cacs=: ];._2 fread BELLCORE,'newestacs'
 pdn=: 0". (11+i.7){"1 cacs
 pdn=: pdn + (10000>pdn){19000000 20000000
 ((x >pdn)*. y <pdn)#cacs
)

   badacm=: 3 : 0
NB. BADAreaCodeMask (badacm)

NB. x is table of area code changes -- olda/c ; exchange ; newa/c
NB. y is enclosed set of phone numbers from list (e.g. 3{"1 table fread 'faxlist')

  'Requires left argument - table of olda/c ; exchange ; newa/c'
:
  (0 1 2 3 4 5 {"1 > digits &.> y) e. 0 1 2 4 5 6 {"1 x
)

   dfixacs=: 3 : 0
NB. DatedFIXAreaCodeS (dfixacs)
NB. x is table of area code changes -- olda/c ; exchange ; newa/c; permit ; manda
NB. y is enclosed set of phone umbers from list (e.g. 3{"1 table fread 'faxlist')

  'Requires left argument - matrix of olda/c ; exchange ; newa/c ; permit ; mandate'
:
  m=. (0 1 2 3 4 5 {"1  fnums=. digits  "1> y) e. 0 1 2 4 5 6 {"1 x
  nac=. (mac=. (0 1 2 4 5 6{"1 x) i. 0 1 2 3 4 5 {"1  m#fnums){8 9 10 {"1 x
  ((<@digits"1 nac ,. 0 3}. m#fnums)(m#i.#m)}y),. ((m#y) ,. mac { <;._2"1 ] (12+i.14){"1 x) (m#i.#m) } ((#m),3)$<''
)

   trimfields=: 3 : 0
  ((0{$y),10){.((32*0=a.i.y)+a.i.y){a.
)

    NB. Infixed here is a side-effect verb used to pick off interesting info
   domore=: 3 : 0
phns=. ''
NB. phns=. phns, > _2{"1 y
)

    NB. The above verb is inserted in a loop of dbf2imp.
   dbf2imp=: 3 : 0
NB. verb to convert a *.DBF file (y) to a Tab Delimited file (x)   
   (y,'.imp') dbf2imp y
:
     w=. 256&#.  NB. A utility to convert bytes to words
NB. Setup variables and file information.
   fs=. fsize y
   bs=. 100000 NB. BlockSize to process per iteration (perhaps whole file)

   buf=. ir y ;0, fp=. bs <. fs - 1 NB. get the first block, set file pointer.
   dbheader=. a.i. 32{.buf
   headlength=. w 9 8{ dbheader
   reclength=. w 11 10{ dbheader
   fields=. }. (>.((headlength-2)%32),32)$buf
   flengths=. a.i. 16&{"1 fields
   fields=. trimfields fields       NB. get just the "clean" names.
   ot=. table unbdos (, fields,. DL), LF
   (table2csv ot) fwrite x  NB. Create an output file, with header info.
   buf=. headlength}. buf           NB. Set buffer to initial data
   fcut=. 1 (_1, _1++/\flengths)}(reclength-1)#0  NB. cuts for fields
NB. Actual work starts here   
  while. (reclength <: # buf)  do.
   data=. (<.((#buf)%reclength),reclength)$buf  NB. Matrix of data
   buf=. (*/$data) }. buf
   data=. 0 1}.(' '= ,0&{"1 data)#data     NB. Remove "deleted" records
   data=. fcut <;.2 "1 data     NB. box each field in table data.
              NB. ntb removes trailing blanks in fields
   (table2csv ntb &.> data) fappend x NB. add this bit of data to output
NB.  domore data  NB. Possible statistics accumulation.
   buf=. buf , ir y ; fp , bs <. fs - fp   NB. Get the next block of data.
   fp=. fp + bs <. fs - fp + 1              NB. logic to end file correctly
  end.
)

   tabledbf=: 3 : 0
NB. verb to convert a *.DBF file (y) to a table in J 
     w=. 256&#.  NB. A utility to convert bytes to words
NB. Setup variables and file information.
   fs=. fsize y
   bs=. 200000 NB. BlockSize to process per iteration (perhaps whole file)
   buf=. ir y ;0, fp=. bs <. fs - 1 NB. get the first block, set file pointer.
   dbheader=. a.i. 32{.buf
   headlength=. w 9 8{ dbheader
   reclength=. w 11 10{ dbheader
   fields=. }. (>.((headlength-2)%32),32)$buf
   flengths=. a.i. 16&{"1 fields
   fields=. trimfields fields       NB. get just the "clean" names.
   ob=. ,: nob &.> <"1 fields  NB. Create an output buffer, with header info
   buf=. headlength}. buf           NB. Set buffer to initial data
   fcut=. 1 (_1, _1++/\flengths)}(reclength-1)#0  NB. cuts for fields
NB. Actual work starts here
  while. (reclength <: # buf)  do.
   data=. (<.((#buf)%reclength),reclength)$buf  NB. Matrix of data
   buf=. (*/$data) }. buf
   data=. 0 1}.(' '= ,0&{"1 data)#data     NB. Remove "deleted" records
   data=. fcut <;.2 "1 data     NB. box each field in table data.
              NB. ntb removes trailing blanks in fields
   ob=. ob,  ( ntb &.> data)  NB. add this bit of data to output
NB.  domore data  NB. Possible statistics accumulation.
   buf=. buf , ir y ; fp , bs <. fs - fp   NB. Get the next block of data.
   fp=. fp + bs <. fs - fp + 1              NB. logic to end file correctly
  end.
ob
)

fixup=: 3 : 0
NB. This fixup verb replaces AreaCodes according to x
NB. x is table of area code changes -- olda/c ; exchange ; newa/c
NB. y is boxed data assuming fax phone is tightly packed (e.g. "2014321987 "
NB. in the "field" third from the end (_3{ )

:
       m=. (0 1 2 3 4 5 {"1 > fnums=. _3{"1 y) e.  ;"1 (0 1{"1 x)

   if. (0 ~: +/m)    do.
       nac=. ((;"1 (0 1{"1 x)) i. 0 1 2 3 4 5 {"1 > m#fnums){>2{"1 x
       ffnums=. ((<"1 nac) ([ , 3: }. ])&.> m# fnums) (m#i.#m)} fnums
NB.    phns=. phns, > ffnums NB. Collect some statistics
(0 _3}. y) ,. ffnums ,. 0 6 }. y
   else. 
NB.    phns=. phns, > fnums  NB. Collect some statistics
       y
   end.
)

   acfixdbf=: 3 : 0
   ('fixed.',y) acfixdbf y
:
     w=. 256&#.  NB. A utility to convert bytes to words
NB. Setup variables and file information.
   fs=. fsize y
   bs=. 100000 NB. BlockSize to process per iteration (perhaps whole file)

   buf=. ir y ;0, fp=. bs <. fs - 1 NB. get the first block, set file pointer.
   dbheader=. a.i. 32{.buf
   headlength=. w 9 8{ dbheader
   reclength=. w 11 10{ dbheader
   fields=. }. (>.((headlength-2)%32),32)$buf
   flengths=. a.i. 16&{"1 fields
   fields=. trimfields fields  NB. get just the "clean" names.
   (headlength {. buf) fwrite x  NB. Create an output file, with headers.
   buf=. headlength}. buf        NB. Set buffer to initial data
   fcut=. 1 (_1,+/\flengths)}reclength#0  NB. cuts for fields
NB. Actual work starts here   
   while. (reclength <: # buf)

      do.
   data=. (<.((#buf)%reclength),reclength)$buf  NB. Matrix of data
   buf=. (*/$data) }. buf
NB.   data=. (' '= ,0&{"1 data)#data     NB. Remove deleted records
   data=. fcut <;.2 "1 data NB. box fields
data=. acdb fixup data                  NB. verb fixup to be modified as needed.
   (; data) fappend x
   buf=. buf , ir y ; fp , bs <. fs - fp   NB. Get the next block of data.
   fp=. fp + bs <. fs - fp + 1              NB. logic to end file correctly
      end.
   buf fappend x   NB. write the rest of the buffer (prolly just EOF)   
)

  bbac=: 3 : 0  NB. Break y (xxx fbl) by areacodes
   3 bbac y  NB. column 3 is default fax number field.
:
     ow=. 3 : (':'; '(; ''|'' untable x) 1!:3 <y')
   tb=. table host 'cat ',y
   acs=. <"1 ] 0 1 2{"1 > x {"1 tb
   acb=. acs </. tb
   act=. table fread BELLCORE,'nacs'
   aci=. (0{"1 act) i. ~. acs
   acm=. aci < #act
        if. (*./acm)  do.
		acn=. 1{"1 aci{act
	else.
                acb=. (acm#acb), < ;"1 (-.acm)#acb
                acx=. ((acm#aci){act),'xxx';'BAD';'Defective';'0'
		acn=. 1{"1 acx
		acs=. 0{"1 acx
        end.
   host 'rm -f A-*'  NB. clear out old lists in PWD...
      if. (x=.) do.  NB. error (intentional??) no idea jkt 20185019
              acf=. 13 : '''A-'',y,x,''.fbl'''
      else.   acf=. 13 : '''A-'',y,x'   end.
   acns=. (~.acs) acf &.> acn
   acbs=. (;"1)&.> acns </.acb
   tmp=. acbs ow&.> ~.acns
   host 'wc -l A-*' 
)

   bbst=: 3 : 0 NB. Break y (xxx fbl) by "state" (remove suffix)
   3 bbst y  NB. column 3 is default fax number field.
:
     ow=. 3 : (':'; '(; ''|'' untable x) 1!:3 <y')
   tb=. table host 'cat ',y
   acs=. <"1 ] 0 1 2{"1 > x {"1 tb
   acb=. acs </. tb
   act=. table fread BELLCORE,'nacs'
   aci=. (0{"1 act) i. ~. acs
   acm=. aci < #act
        if. (*./acm)  do.
                acn=. 1{"1 aci{act
        else.
                acb=. (acm#acb), < ;"1 (-.acm)#acb
                acn=. 1{"1 ((acm#aci){act),'xxx';'BAD';'Defective';'0'
        end.
   host 'rm -f S-*' NB. clear out old lists in PWD...
      if. (x=.) do. NB. error (intentional??) no idea jkt 20185019
              state=. 13 : '''S-'',y,''.fbl'''
      else.   state=. 13 : '''S-'',y'   end.
   acns=.    state &.> acn
   acbs=. (;"1)&.> acns </.acb
   tmp=. acbs ow&.> ~.acns
   host 'wc -l S-*'
)

   bbtz=: 3 : 0 NB. Break y (xxx fbl) by "state" (remove suffix)
   3 bbtz y  NB. column 3 is default fax number field.
:
     ow=. 3 : (':'; '(; ''|'' untable x) 1!:3 <y')
   tb=. table host 'cat ',y
   acs=. (3&{.)&digits &.> x {"1 tb
   acb=. acs </. tb
   act=. table fread BELLCORE,'nacs'
   aci=. (0{"1 act) i. ~. acs
   acm=. aci < #act
        if. (*./acm)  do.
                acn=. _1{"1 aci{act
        else.
                acb=. (acm#acb), < ;"1 (-.acm)#acb
                acn=. _1{"1 ((acm#aci){act),'xxx';'BAD';'Defective';'XX'
        end.
   host 'rm -f Z+*' NB. clear out old lists in PWD...
      if. (x=.) do. NB. error (intentional??) no idea jkt 20185019
              tzn=. 13 : '''Z+'',y,''.fbl'''
      else.   tzn=. 13 : '''Z+'',y'   end.
   acns=. tzn &.> acn
   acbs=. (;"1)&.> acns </.acb
   tmp=. acbs ow&.> ~.acns
   host 'wc -l Z+*'
)

 nbast=: 3 : 0 NB. Names by AC, State, Timezone from boxed phone numbers
   0 nbast y  NB. 0=.AC  --  1=.ST  --  _1=.TZ  Default=.AC
:
   acs=. (3&{.)&digits &.> y
   act=. table fread BELLCORE,'nacs'
   aci=. (0{"1 act) i. acs
   acn=. (((x=.)#1),x){"1 aci{act,'xxx';'BAD';'Defective';'XX'
   <@;"1 (x { 'AC-';'ST-';'TZ-'),.acn
)

 packlist=: 3 : 0
NB. Assumes y a boxed table with fields:
NB. First, Last, Company, Fax, Voice, SubID, Banner, Path, ListID
NB. Finds groups where Fax, Banner are duplicated and minimizes the
NB.  number of faxes by putting multiple names in First and Last
NB.  where the field length is limited to 40 characters.
  3 6 _1 packlist y  NB. as assumed above --  3 _1 is interesting too.
:
 groups=. (x {"1 y) </. y
 fixgroups=. (1 < tg=: ; #&.> groups) # groups
 ; (packgroup &.> fixgroups) , (1 = tg) # groups
)

 makebanners=: 3 : 0
lists=. 8{"1 y
lines=. (m=: ~: lists) # 6 {"1 y
lists=. (] , '.bnr'"_) &.> m#lists
lines=. (] , NL"_) &.> lines
lines fwrite &.> lists
)

 packgroup=: 3 : 0
NB. Takes a boxed list of boxed tables in form assumed by packlist
NB.  and packs them into a minimal set.
 names=. ntb &.> <&;"1 [0 _1 1 {"1 y,. <' '
 ln=. 2 + ; $&.> names
 nnames=. ((</\(40 * 1+i. >. (+/ln)%40) >/ +/\ ln) <@# names,.<', '),<''
 tnames=. ;&.> |."1 ((1 >. <.(#nnames)%2),2)$,nnames
 tnames=. |:(0{"1 tnames),: (_2&}.)&.> 1{"1 tnames
 tnames,"1 [ 2}."1 (0{ y)

)

    tzlists=: 3 : 0
'Usage: ''ListID'' tzlists pns  NB. pns is boxed list'
:
act=. table fread BELLCORE,'nacs'
acs=. (3&{.)&digits &.> y
aci=. acs i.~ 0{"1 act
(<x),&.>aci{(_1{"1 act),<'00'
)

NB. Export Utilities - 
scat=: 3 : 0
''scat y
:
(b e.95{.33}.a.)#b=: x,y
)

 export_lists=: 3 : 0
'.fbl' export_lists y
:
t=. <;._2 ;._2 ]1!:1<y  NB. Read y and build a table of fields.
i=. (lcname&.><"1;"1 ]_2 _1{"1 t)scat&.><x NB. Last col is List ID next to last path.
in=. (<'/netdisk/dlists/')([ , 8: }. ])&.> ~. i NB. A list of files.
fns=. CACS dfixacs 3{"1 t
echo 'Areacodes updated: ' , ": +/ CACS badacm 3{"1 t
t=. (0 1 2{"1 t),. (pncompress &.> (0{"1 fns),. 4{"1 t),. 0 5}. t NB. Make pretty #s
ot=. (mi=. ~: lcname"1 ;"1 ]0 1 3 _2 _1{"1 t)#t
echo (":(#t)-#ot),' duplicates removed.'
echo 'Remaining duplicate numbers within lists -'
echo ": shist dist lcname"1 ;"1 ] 3 _2 _1{"1 ot
list=. '|'&untable &.> (mi#i) </. ot  NB. One item in "list" for each ID.
list (>@[ 1!:2 <@(>@]))"0 in NB. Write all the files.
(":#ot),' records written as ',(":#list),' files'    NB. The result...
)

  fbl2fdsf=: 3 : 0
b=. (($y)>.0 5){. y
m=. '#' ~: 0{"1 ' ',.~>fl=. <&ndb"1 ;"1 ]0 _1 1{"1 b,. <' '
flc=. <&ndb "1 (($flc)<. _ 31){.flc=. ;"1 ] 3 _1 0 {"1 fl,. b,. <' > '
ot=. m#((] , ' '"_)&.> 3{"1 b),.flc,.fl,.0 2}.b
)

  fbl2fds=: 3 : 0
('|' untable fbl2fdsf table fread y) fwrite (_3}.y),'fds'
)

  u2m=: 3 : 0
((LF;CR)swapin fread y) fwrite y
)

  m2u=: 3 : 0
((CR;LF)swapin fread y) fwrite y
)

  fswap=: 4 : 0
(x swapin fread y) fwrite y
)

  urm=: 3 : 'host ''rm '',y'
  amdupi=: 3 : 0
  1 amdupi y NB. 1 is default At Most DUP Index
:
 if. (1 ~: x) do.
  i=. (dm=:y e.~.(-.~:y)#y)#i.#y
  /:~(;(dm #y) x &([: < ([ <. [: # ]) {. ])/. i),(-. dm)#i.#y
 else. (~: y)#i.#y
 end.
)

  getlines=: 3 : 0
 100000 getlines y  NB. Default BS is 100,000 bytes
:
 bs=. x
 fs=. fsize fn=. > 0{y
 fl=. bs <. fs -fp=. > 1{y
 buf=. ir fn;fp,fl
  if. (fs=.fp=. fp + fl) do. fp=. _1 end.
 drop=. (<:#buf)-buf i: NL 
  if. ((drop ~: 0) *. fp=._1 ) do. echo '** Unexpected EOF **' end.
 fp=. _1 >. fp - drop
 fn;fp;buf }.~ -drop
)


   dbf2txt=: 3 : 0
NB. verb to convert a *.DBF file (y) to a Tab Delimited file (x)
   (y,'.txt') dbf2txt y
:
     w=. 256&#.  NB. A utility to convert bytes to words
NB. Setup variables and file information.
   fs=. fsize y
   bs=. 100000 NB. BlockSize to process per iteration (perhaps whole file)

   buf=. ir y ;0, fp=. bs <. fs - 1 NB. get the first block, set file pointer.
   dbheader=. a.i. 32{.buf
   headlength=. w 9 8{ dbheader
   reclength=. w 11 10{ dbheader
   fields=. }. (>.((headlength-2)%32),32)$buf
   flengths=. a.i. 16&{"1 fields
   fields=. trimfields fields       NB. get just the "clean" names.
   ot=. table unbdos (, fields,. DL), LF
   NB. ((;flengths {.&.> ,ot),NL) fwrite x  NB. Create an output file, with header info.
   (ndb ; (fields makeftabs flengths),. <NL) fwrite x, '.js' 
   buf=. headlength}. buf           NB. Set buffer to initial data
   fcut=. 1 (_1, _1++/\flengths)}(reclength-1)#0  NB. cuts for fields
NB. Actual work starts here
  while. (reclength <: # buf)  do.
   data=. (<.((#buf)%reclength),reclength)$buf  NB. Matrix of data
   buf=. (*/$data) }. buf
   data=. 0 1}.(' '= ,0&{"1 data)#data     NB. Remove "deleted" records
   (, data,. NL) fappend x NB. add this bit of data to output
   buf=. buf , ir y ; fp , bs <. fs - fp   NB. Get the next block of data.
   fp=. fp + bs <. fs - fp + 1              NB. logic to end file correctly
  end.
)

   makeftabs=: 3 : 0
:
(<"1 x),.(<'=: '),. (<"1 ": ,. 0,}: +/\y),. (<' + i. '),. < "1 ": ,. y
)

   rjust=: ] |."0 1~ [: >: 1: i:"1~ ' '"_ ~: ]

   fproc=: 3 : 0
fb=. y ;0
 whilst. (0 < fp=. >1{fb) do.
  buf=. > _1{ fb=. getlines fb
  process buf
 end.
fp
)

   findlostrans=: 3 : 0
'x is boxed vector of FaxIDs to find and put into y,.out'
:
 buf=. box nob host 'cut -d: -f2 ', y
   m=. buf e. ns
  echo y,': ',":+/m
     if. (0~: +/m) do. m fcompress y
     end.
)

   fcompress=: 3 : 0
echo 'Usage: boolean compress file  -- writes file.out'
:
buf=. y ;0 
host 'rm ',y,'.out'
  whilst. (0<>1{buf) do. 
  buf=. getlines buf
   ob=. <;.2 >2{buf
    (;((#ob){. x) # ob) fappend y,'.out'
    x=. (#ob) }. x
  end.
)

   fsplit=: 3 : 0
echo 'Usage: boolean "de-shuffle" a file into file.0 and file.1'
:
buf=. y ;0
host 'rm ',y,'.0 ',y,'.1'
  whilst. (0<>1{buf) do. 
  buf=. getlines buf
   ob=. <;.2 >2{buf
      (;((#ob){. x) # ob) fappend y,'.1'
      (;(-.(#ob){. x) # ob) fappend y,'.0'
      x=. (#ob) }. x
   end.
)


  tadjust=: 13 : '(0~:y)*18+6*>.y %6 ' 

  rand=: 13 : '(?~#y){y'
  rf=: 13 : '(x ?#y){y'

  hnt=: 13 : '(x |.}:i:x){y'
  
swapin=: 3 : 0
'Requires pair of items (in x) to swap in y'
:
('out';'in')=. x
m=. (1{.~#out), out E. y
z=. (#out)&}. &.> m<;.1 out,y
(#in)}.;,(<in),.,.z
)

  trnd=:  6: * [: >. ] % 6:

  nfield=: 3 : 0
'x is file name, y is field number (counting from 1)=.result is numeric'
:
 0".];._2 host 'cut -d: -f',(":y),' ',x
)
   scsv=: 3 : 0
   ',' scsv y
:
   (-.'"'e.y) +. (+/('"',x,'"')E.y)=+/x=.  NB. error (intentional??) no idea jkt 20185019
)

   csv2txt=: 3 : 0
 host 'rm -f ',y,'.tab'
 buf=. y ;0
  whilst. (0<>1{buf) do.
   buf=. 64000 getlines buf
    if. (scsv b=. >2{buf) do.
       tb=. ',' table b -. '"'
    else.
       tb=. tablecsv b
    end.
   (untable tb) fappend y,'.tab'
   end.
)

   jfd=: 3 : 0 "1
'y m d'=. y
ya=. y - m <: 2
c=. <. ya % 100
dc=. <. 36524.25 * c
dy=. <. 365.25 * ya - 100 * c
dm=. <. 0.4 + 30.6 * 12 | m - 3
j=. 1721119 + dc + dy + dm + d
)

   dfj=: 3 : 0 "0
x=. y - 1721119
c=. <. (x - 0.25) % 36524.25
cd=. <. _0.25 + x - c * 36524.25
yy=. <. (cd + 0.75) % 365.25
dd=. <. 1.75 + cd - 365.25 * yy
mm=. <. (dd - 0.6) % 30.6
d=. <. 0.4 + dd - mm * 30.6
m=. 1 + 12 | 2 + mm
y=. (c * 100) + yy + mm >: 10
y, m, d
)


  dts=: 3 : 0 "1
NB. Let y be the year month day [hour minute second] in the Gregorian calendar
  BD=. 2440588 -~ jfd 3 {. y
NB. The BDOS day number (days since 1970/1/1)
NB. was BDTS=. 43200 + (0 60 60 #. 3{. 3}. y)+86400*BD  NB. 2018/8/8
  (0 60 60 #. 3{. 3}. y)+86400*BD
)

  dtv=: 3 : 0 "0
NB. Calendar date from dts (a scalar number of seconds since 1970 1 1 0 0 0 )
  BD=. {. ts=. 0 24 60 60 #: y
NB. was (dfj BD+2440587.5), }. ts  NB. 2018/8/8
  (dfj BD+2440588), }. ts 
)

NB. Some global variables that are needed by various scriptes.

NB.  CACS=: currentacs 960601 NB. default areacode updater

   strike=: 3 : 0 "1
   y strike ~.y  NB. of unknown usefulness, sort of anti ~.
:
      kx=. x -. y
    if. (*./~:x) do. return. end. NB. Generally not worth the test...
      cx=. #/.~ sx=. /:~(x e. y)#x
      cy=. #/.~ /:~(y e. x)#y
      z=. kx, (0 >. cx - cy)# ~. sx
    x (([: /: i:) { ]) z  NB. restores order of x (not really, sigh jkt 2004 5 18)
)

   strike1=: 4 : 0
 rankx=: i.~ (] - {) /:@/:  NB. RHui from jforum 2002/2/18
 i=. x i. x,y
 ix=. (#x){.i
 iy=. (#x)}.i
 x #~ (#y)=. (iy,.rankx iy) i. (ix,.rankx ix)  NB. error (intentional??) no idea jkt 20185019
)


NB.=.========================================= jkt 2004/3/13 vv
NB. tried (unsuccessfully) in incorporate into system/main/conlib.ijs

host=: 13 : '2!:0 ''('',y,'' || true)''' NB. ALWAYS return a result...

NB. --- following doesn't work since boot.ijs isn't yet loaded at this time...
NB. require 'libpath'
NB. 
NB. NB. conditional definitions
NB. 3 : 0''
NB. if. IFUNIX do.
NB.   lib=. find_dll 'c'
NB.   lib=. lib, (>: (<UNAME) e. ;:'SunOS NetBSD') {. ' _'
NB.   api=. 1 : ('(''',lib,''',x) & cd')
NB.   c_isatty=: ' isatty i i' api
NB. end.
NB. i. 0 0
NB. )
NB. --- so temporarily put into system/main/hbu ijs

NB.  On 2012/01/16 08:07 , bill lam wrote: Joey Tuttle added c_isatty to jmf package, 
NB.  as far as I can see, this api was only used in his getargs verb. 
NB.  The new J base library added isatty and getargs which use the renamed isatty.  
NB.  I propose to remove c_isatty from jmf package if it is not used in other places.
NB. --- therefore (thankfully) I can comment out my getargs verb  jkt 2015/03/15

NB.   getargs=: 3 : 0
NB.   ARGV getargs y
NB. :
NB. didn't work, too bad ...  require 'hbu' NB. HashBangUtilties - mainly c_isatty_hbu_
NB.   require 'jmf'
NB.   argb=. (]`(([: < 1: {. }.) , [: < 2: }. ])@.('-'"_=.{.))&.> x
  NB. The above boxes parms (elements starting with "-" returning name;value
NB.   parm=. 32=.;(3!:0)&.> argb
NB.   ((-. parm)#argb);(>parm#argb);(". ({. 0=.>c_isatty_jmf_ 0)#'stdin ''''')
NB. )

  msplit=: 3 : 0
  1e8 msplit y
:
  bs=. x + spltno=. fp=. # buf=. ''
  fs=. fsize fn=. y
while. (0 < rl=. bs <. fs -fp ) do.
  buf=. buf, ir fn ; fp, rl
  fp=. fp + rl
  wii=. {: (bs > ii) # ii=. I. (LF,'From ') E. LF, buf
echo (#buf), wii, (0 ~: {. ii) # '** Corrupt Buffer **'
  (wii{.buf) fwrite 'split', ": spltno=. >: spltno
  buf=. wii}.buf
end.
  buf fappend 'split', ": spltno
)

  mbox=: 13 : '(1, }. (LF, ''From '') E. y) <;.1 y'


NB.=.=========================================== jkt 2004/3/13 ^^

NB. Just for fun ...

  wwords=: 3 : 0
3 wwords y
:
dictpath=. '/Users/jkt/Documents/jstuff/words/'
sla=. 'abcdefghijklmnopqrstuvwxyz-''' -. y
 buf=.  fread dictpath,'dict'  NB. Assume that dict is in pwd
if. 0=.$$x do.  NB. true if default (range x - #y) else just specified x lengths
 buf=. (] #~ a: ~: ]) ((x }. i. 1+#y) e.~ <:2 -~/\ I.1,LF=buf)# <;._2 buf
else.
 buf=. (] #~ a: ~: ]) (x e.~ <:2 -~/\ I.1,LF=buf)# <;._2 buf
end.
z=. /:~>(-. ; +./&.> buf e. &.> <sla)#buf
z=. (*/"1 ' '=z strike y)#z
z\:+/"1' '=z
)

  jwords=: 3 : 0
(#y) jwords y
:
if. x=#y do. x wwords y else.
assert. 2=.#~.x  NB. wish we could generalize to more than 2 words and equal length ones.....
dictpath=. '/Users/jkt/Documents/jstuff/words/'
sla=. ('''-', al=. 'abcdefghijklmnopqrstuvwxyz') -. y NB. to remove words with non-conforming letters
wbuf=. (-. ; +./&.> tbuf e. &.> <sla)# tbuf=. (] #~ a: ~: ]) (x e.~ <:2 -~/\ I.1,LF=buf)# <;._2 buf=. fread dictpath,'dict'
(;bs=.(' b'&,)&": &.> ~.#&.>wbuf)=.(;#&.>wbuf)</.wbuf
   pwv=. 13 : '*/x: (p: i. 26) {~ ((97+i. 26){a.) i. y'  NB. Prime Word Value 
mi=. 0~: 0{"1  ii=. I. (pwv y)=.". _2}. ,('(;pwv &.>',"1 >bs),"1 ')*/'
untable (mi# ". >0{bs),. ((0=mi#ii) -~ mi#ii){a:,~ ". >1{bs 
end.
NB. (*/"1 ' '=(1 0 2|: z6,"1 /  z4)strike"2 1 y) -: (*/x: pf {~ al i. y)=(*/"1  pf {~ al i. z4) */ */"1  pf {~ al i. z6
)

NB. some benchmarks
   mibm=: 3 : 0
echo JVERSION  NB. probably want |: mibm  10  (to run JKT bench 10 times)
z=. 0 0$a:
  for_ms. 50 100 500 1000 do.
   exp=. '%. ', (": 2#ms),' ?@$ 10000'
   z=.z, exp; ":&.> y timex exp
  end.
)


   Pi=: (2: + j.~) ": [: (<.@o. % ]) 10"_ ^ x:

NB.  now change back to the 'home' locale & add jkt to its path (again explicitly)
18!:4 <'base'                     NB.  cocurrent'base'
('jkt';18!:2<'base')18!:2<'base'  NB.  ('jkt';copath'base')copath'base'

CONSOLEOUTPUT_jzplot_=: 'pdf'

NB.  debug 0  NB. hopefully we get here successfully. failed experiment to catch spellig errors.
