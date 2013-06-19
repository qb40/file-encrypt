DECLARE FUNCTION decodestring$ (str1$, pass$)
DECLARE FUNCTION encodestring$ (str1$, pass$)
DECLARE FUNCTION boxinput$ (n$, y%, pass%)
DECLARE SUB start ()
DECLARE SUB draws ()
DECLARE SUB clears ()
DECLARE SUB encode ()
DECLARE SUB decode ()
DECLARE SUB statusbox (n$, y%, size%)
DECLARE SUB percent (done&, total&, y%)





'Declaring Keys
CONST backspc = 8, enter = 13, htab = 9, esc = 27
CONST left = 75, right = 77, up = 72, down = 80
CONST uplt = 71, uprt = 73, dnlt = 79, dnrt = 81
CONST insert = 82, home = 73, pageup = 71, del = 83, endk = 81, pagedn = 79
CONST kf1 = 59, kf2 = 60, kf3 = 61, kf4 = 62, kf5 = 63, kf6 = 64, kf7 = 65, kf8 = 66, kf9 = 67, kf10 = 68, kf11 = 133, kf12 = 134
'Keys declared

CONST loadsize = 1000












CLS
draws
GOTO mainmenu

comeback:
SELECT CASE sel%
CASE 0
clears
LOCATE 5, 25
COLOR 14, 0
PRINT "ENCODE FILE:";
COLOR 15, 1
file$ = boxinput$("File Name:", 6, 0)
COLOR 7, 0
LOCATE 20, 4
PRINT "*  Longer passwords give better protection.";
LOCATE 21, 4
PRINT "** Multi-encoded file are more secure."
DO
COLOR 15, 1
pass$ = boxinput$("Password:", 6, 1)
COLOR 15, 1
pas2$ = boxinput$("Retype Password:", 6, 1)
COLOR 7, 2
LOCATE 18, 6
PRINT "Password doesn't match.";
LOOP UNTIL pass$ = pas2$
clears
COLOR 15, 1
out$ = boxinput$("Encoded file name:", 6, 0)
start
encode
CASE 1
clears
LOCATE 5, 25
COLOR 14, 0
PRINT "DECODE FILE:";
COLOR 15, 1
file$ = boxinput$("Encoded File Name:", 6, 0)
COLOR 7, 0
LOCATE 20, 4
PRINT "*  An Encoded file may have password of 1-55 characters.";
LOCATE 21, 4
PRINT "** An Encoded file may be Multi-Encoded file."
DO
COLOR 15, 1
pass$ = boxinput$("Password:", 6, 1)
COLOR 15, 1
pas2$ = boxinput$("Retype Password:", 6, 1)
COLOR 7, 2
LOCATE 18, 6
PRINT "Password doesn't match.";
LOOP UNTIL pass$ = pas2$
clears
COLOR 15, 1
out$ = boxinput$("Decoded file name:", 6, 0)
start
decode
CASE ELSE
END SELECT







mainmenu:
clears
LOCATE 4, 25, 0
PRINT "Select Option:";
sel% = 0
GOSUB dispoptmain
DO
k$ = ""
WHILE k$ = ""
k$ = INKEY$
WEND
SELECT CASE k$
CASE CHR$(esc)
CLS
COLOR 15, 0
PRINT "Thank You for using ENCODER."
PRINT
PRINT
PRINT "developed by: Subhajit Sahu"
k$ = INPUT$(1)
SYSTEM
CASE (CHR$(0) + CHR$(up))
sel% = (sel% + 1) MOD 2
GOSUB dispoptmain
CASE (CHR$(0) + CHR$(down))
sel% = (sel% + 1) MOD 2
GOSUB dispoptmain
CASE ELSE
END SELECT
LOOP UNTIL k$ = CHR$(enter)
GOTO comeback























dispoptmain:
IF (sel% = 0) THEN
COLOR 15, 14
LOCATE 5, 30, 0
PRINT "- Encode File";
COLOR 15, 0
LOCATE 6, 30, 0
PRINT "- Decode File";
ELSE
COLOR 15, 0
LOCATE 5, 30, 0
PRINT "- Encode File";
COLOR 15, 14
LOCATE 6, 30, 0
PRINT "- Decode File";
END IF
RETURN

FUNCTION boxinput$ (n$, y%, pass%)
COLOR 13, 4
LOCATE y%, 10, 0
PRINT "Õ"; STRING$(58, "Í"); "¸";
LOCATE y% + 1, 10, 0
PRINT "³ "; n$; STRING$(57 - LEN(n$), " "); "³";
LOCATE y% + 2, 10, 0
PRINT "Ã"; STRING$(58, "Ä"); "´";
LOCATE y% + 3, 10, 0
PRINT "³"; STRING$(58, " "); "³";
LOCATE y% + 4, 10, 0
PRINT "À"; STRING$(58, "Í"); "Ù";
COLOR 14, 0
y1% = y% + 3
LOCATE y1%, 12, 0
PRINT STRING$(56, " ");
LOCATE y1%, 12, 1
a$ = ""
DO
k% = ASC(INPUT$(1))
IF (k% = backspc) THEN
IF (LEN(a$) <> 0) THEN
x% = 11 + LEN(a$)
LOCATE y1%, x%, 0
PRINT " ";
a$ = LEFT$(a$, LEN(a$) - 1)
x% = x%
LOCATE y1%, x%, 1
END IF
ELSEIF (k% = esc) THEN
a$ = ""
LOCATE y1%, 12, 0
PRINT STRING$(56, " ");
LOCATE y1%, 12, 1
ELSEIF (k% = enter) THEN
EXIT DO
ELSEIF (k% >= 32 AND k% <= 126) THEN
IF (LEN(a$) < 55) THEN
b$ = CHR$(k%)
x% = 12 + LEN(a$)
LOCATE y1%, x%, 0
IF (pass%) THEN PRINT "è";  ELSE PRINT b$;
a$ = a$ + b$
x% = x% + 1
LOCATE y1%, x%, 1
END IF
END IF
LOOP
COLOR 7, 0
FOR k% = 0 TO 4
LOCATE y% + k%, 10
PRINT STRING$(60, " ");
NEXT
boxinput$ = a$
END FUNCTION

SUB clears
COLOR 15, 0
FOR i% = 4 TO 23
LOCATE i%, 2
PRINT SPACE$(78);
NEXT
END SUB

SUB decode
SHARED file$, pass$, out$, oldchksum&, newchksum&
OPEN "B", #1, file$
OPEN "B", #2, out$
total& = LOF(1)
IF (total& = 0) THEN
clears
LOCATE 8, 5
COLOR 7, 2
PRINT "File "; file$; " does not exist.";
GOTO enddecode
END IF
dat$ = INPUT$(7, #1)
IF (dat$ <> "ENCODED") THEN
COLOR 7, 2
LOCATE 9, 7, 0
PRINT "File "; file$; " is not an Encoded file.";
LOCATE 10, 7, 0
PRINT "Press any key";
k$ = INPUT$(1)
GOTO enddecode
END IF
chksum$ = INPUT$(8, #1)
p& = 1
statusbox "DECODING:", 5, 9
COLOR 7, 2
LOCATE 9, 7, 0
PRINT "Decoding of file "; file$; " is in progress.";
LOCATE 10, 7, 0
PRINT "Decoded file is "; out$; ".";
total& = LOF(1) - 16
done& = 0
DO
dat$ = INPUT$(loadsize, #1)
dat$ = decodestring$(dat$, pass$)
PUT #2, p&, dat$
p& = p& + loadsize
done& = done& + loadsize
percent done&, total&, 15
LOOP UNTIL LEN(dat$) < loadsize
chkgot$ = MKL$(oldchksum&) + MKL$(newchksum&)
CLOSE #1, #2
IF (chkgot$ = chksum$) THEN
COLOR 7, 2
LOCATE 9, 7, 0
PRINT "Decoding of file "; file$; " is complete.   ";
LOCATE 10, 7, 0
PRINT "Decoded file is "; out$; ".";
LOCATE 11, 7, 0
PRINT "Press any key"
k$ = INPUT$(1)
ELSE
KILL out$
clears
COLOR 7, 2
LOCATE 9, 7, 0
PRINT "FATAL ERROR: File "; file$; " could not be decoded.";
LOCATE 10, 7, 0
PRINT "File "; out$; " could not be created. Check Password.";
LOCATE 11, 7, 0
PRINT "Press any key";
k$ = INPUT$(1)
END IF

enddecode:
END SUB

FUNCTION decodestring$ (str1$, pass$)
SHARED passpoint%, oldchksum&, newchksum&
b$ = ""
FOR i% = 1 TO LEN(str1$)
newchar% = ASC(MID$(str1$, i%, 1))
newchksum& = newchksum& + newchar%
oldchar% = (512 - 128 + (newchar% - ASC(MID$(pass$, passpoint% + 1, 1)))) MOD 256
oldchksum& = oldchksum& + oldchar%
b$ = b$ + CHR$(oldchar%)
passpoint% = (passpoint% + 1) MOD (LEN(pass$))
NEXT
decodestring$ = b$
END FUNCTION

SUB draws
PRINT "É"; STRING$(78, "Í"); "»"
PRINT "º"; STRING$(35, " "); "ENCODER"; STRING$(36, " "); "º"
PRINT "Ì"; STRING$(78, "Í"); "¹"
FOR i% = 4 TO 23
PRINT "º"; STRING$(78, " "); "º"
NEXT
PRINT "È"; STRING$(78, "Í"); "¼";
END SUB

SUB encode
SHARED file$, pass$, out$, oldchksum&, newchksum&
OPEN "B", #1, file$
OPEN "B", #2, out$
total& = LOF(1)
IF (total& = 0) THEN
clears
LOCATE 8, 5
COLOR 7, 2
PRINT "File "; file$; " does not exist.";
GOTO endencode
END IF
dat$ = "ENCODED" + STRING$(8, 0)
PUT #2, 1, dat$
p& = 16
statusbox "ENCODING:", 5, 9
COLOR 7, 2
LOCATE 9, 7, 0
PRINT "Encoding of file "; file$; " is in progress.";
LOCATE 10, 7, 0
PRINT "Encoded file is "; out$; ".";
total& = LOF(1)
done& = 0
DO
dat$ = INPUT$(loadsize, #1)
dat$ = encodestring$(dat$, pass$)
PUT #2, p&, dat$
p& = p& + loadsize
done& = done& + loadsize
percent done&, total&, 15
LOOP UNTIL LEN(dat$) < loadsize
dat$ = MKL$(oldchksum&) + MKL$(newchksum&)
PUT #2, 8, dat$
CLOSE #1, #2
COLOR 7, 2
LOCATE 9, 7, 0
PRINT "Encoding of file "; file$; " is complete.   ";
LOCATE 10, 7, 0
PRINT "Encoded file is "; out$; ".";
LOCATE 11, 7, 0
PRINT "Press any key";
k$ = INPUT$(1)

endencode:
END SUB

FUNCTION encodestring$ (str1$, pass$)
SHARED passpoint%, oldchksum&, newchksum&
b$ = ""
FOR i% = 1 TO LEN(str1$)
oldchar% = ASC(MID$(str1$, i%, 1))
oldchksum& = oldchksum& + oldchar%
newchar% = (oldchar% + ASC(MID$(pass$, passpoint% + 1, 1)) + 128) MOD 256
newchksum& = newchksum& + newchar%
b$ = b$ + CHR$(newchar%)
passpoint% = (passpoint% + 1) MOD (LEN(pass$))
NEXT
encodestring$ = b$
END FUNCTION

SUB percent (done&, total&, y%)
LOCATE y%, 7, 0
COLOR 13, 6
sz% = CINT((done& / total&) * 65 + .5)
IF (sz% > 65) THEN sz% = 65
PRINT STRING$(sz%, "°");
COLOR 15, 0
END SUB

SUB start
SHARED passpoint%, oldchksum&, newchksum&
passpoint% = 0
oldchksum& = 0
newchksum& = 0
END SUB

SUB statusbox (n$, y%, size%)
COLOR 15, 2
LOCATE y%, 5, 0
PRINT "Õ"; STRING$(68, "Í"); "¸";
LOCATE y% + 1, 5, 0
PRINT "³ "; n$; STRING$(67 - LEN(n$), " "); "³";
LOCATE y% + 2, 5, 0
PRINT "Ã"; STRING$(68, "Ä"); "´";
FOR i% = 0 TO size%
LOCATE y% + 3 + i%, 5, 0
PRINT "³"; STRING$(68, " "); "³";
NEXT
ly% = y% + 4 + size%
LOCATE ly%, 5, 0
PRINT "À"; STRING$(68, "Ä"); "Ù";
ly% = ly% - 4
COLOR 14, 3
LOCATE ly%, 6, 0
PRINT "Ú"; STRING$(66, "Ä"); "¿";
LOCATE ly% + 1, 6, 0
PRINT "³"; STRING$(66, " "); "³";
LOCATE ly% + 2, 6, 0
PRINT "À"; STRING$(66, "Í"); "Ù";
END SUB

