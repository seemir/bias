AFnom=nominal(AF);
getlevels(AFnom)
AFnum=[];
for i=1:length(AF)
if AFnom(i)=='no'
AFnum(i)=0;
elseif AFnom(i)=='yes'
AFnum(i)=1;
else
AFnum(i)=10e10;
end
end
AFnum=AFnum';

ARnom=nominal(AR);
getlevels(ARnom)
ARnum=[];
for i=1:length(AR)
if ARnom(i)=='no'
ARnum(i)=0;
elseif ARnom(i)=='normal'
ARnum(i)=0;
elseif ARnom(i)=='mild'
ARnum(i)=1;
elseif ARnom(i)=='moderate'
ARnum(i)=2;
else
ARnum(i)=10e10;
end
end
ARnum=ARnum';

ASnom=nominal(AS);
getlevels(ASnom)
ASnum=[];
for i=1:length(AS)
if ASnom(i)=='no'
ASnum(i)=0;
elseif ASnom(i)=='normal'
ASnum(i)=0;
elseif ASnom(i)=='mild'
ASnum(i)=1;
elseif ASnom(i)=='moderate'
ASnum(i)=2;
else
ASnum(i)=10e10;
end
end
ASnum=ASnum';

asymptomaticforHFnom=nominal(asymptomaticforHF);
getlevels(asymptomaticforHFnom)
asymptomaticforHFnum=[];
for i=1:length(asymptomaticforHF)
if asymptomaticforHFnom(i)=='no'
asymptomaticforHFnum(i)=0;
elseif asymptomaticforHFnom(i)=='yes'
asymptomaticforHFnum(i)=1;
else
asymptomaticforHFnum(i)=10e10;    
end
end
asymptomaticforHFnum=asymptomaticforHFnum';


BPnom=nominal(BP);
getlevels(BPnom)
BPnum=[];
for i=1:length(BP)
if BPnom(i)=='no'
BPnum(i)=0;
elseif BPnom(i)=='yes'
BPnum(i)=1;
else
BPnum(i)=10e10;
end
end
BPnum=BPnum';

Breathlessnom=nominal(Breathless);
getlevels(Breathlessnom)
Breathlessnum=[];
for i=1:length(Breathless)
if Breathlessnom(i)=='no'
Breathlessnum(i)=0;
elseif Breathlessnom(i)=='yes'
Breathlessnum(i)=1;
else
Breathlessnum(i)=10e10;    
end
end
Breathlessnum=Breathlessnum';

Patient_groupnom=nominal(Patient_group);
getlevels(Patient_groupnom)
Patient_groupnum=[];
for i=1:length(Patient_group)
if Patient_groupnom(i)=='A'
    Patient_groupnum(i)=1;
elseif Patient_groupnom(i)=='B'
    Patient_groupnum(i)=1;
%Patient_groupnum(i)=2;
elseif Patient_groupnom(i)=='C'
    Patient_groupnum(i)=2;
%Patient_groupnum(i)=3;
elseif Patient_groupnom(i)=='D'
    Patient_groupnum(i)=3;
%Patient_groupnum(i)=4;
elseif Patient_groupnom(i)=='E'
    Patient_groupnum(i)=4;
%Patient_groupnum(i)=5;
elseif Patient_groupnom(i)=='F'
    Patient_groupnum(i)=4;
%Patient_groupnum(i)=6;
else
Patient_groupnum(i)=10e10;
end
end
Patient_groupnum=Patient_groupnum';

Patient_group=CategoriesINinhosptdeathZdeadwithin30daysYdeadwithin1yearXdead1;
Patient_groupnom=nominal(Patient_group);
lev_pat=getlevels(Patient_groupnom)
Patient_groupnum=[];
for i=1:length(Patient_group)
if Patient_groupnom(i)==lev_pat(1)
    Patient_groupnum(i)=1;
elseif Patient_groupnom(i)==lev_pat(2)
    Patient_groupnum(i)=2;

elseif Patient_groupnom(i)==lev_pat(3)
    Patient_groupnum(i)=3;

elseif Patient_groupnom(i)==lev_pat(4)
    Patient_groupnum(i)=4;

elseif (Patient_groupnom(i)==lev_pat(5) | Patient_groupnom(i)==lev_pat(6) | Patient_groupnom(i)==lev_pat(7))
    Patient_groupnum(i)=5;

elseif (Patient_groupnom(i)==lev_pat(8) | Patient_groupnom(i)==lev_pat(9) | Patient_groupnom(i)==lev_pat(10))
    Patient_groupnum(i)=6;

else
Patient_groupnum(i)=10e10;
end
end
Patient_groupnum=Patient_groupnum';

Chestpainnom=nominal(Chestpain);
getlevels(Chestpainnom)
Chestpainnum=[];
for i=1:length(Chestpain)
if Chestpainnom(i)=='no'
Chestpainnum(i)=0;
elseif Chestpainnom(i)=='yes'
Chestpainnum(i)=1;
else
Chestpainnum(i)=10e10;    
end
end
Chestpainnum=Chestpainnum';

Cholnom=nominal(Chol);
getlevels(Cholnom)
Cholnum=[];
for i=1:length(Chol)
if Cholnom(i)=='no'
Cholnum(i)=0;
elseif Cholnom(i)=='yes'
Cholnum(i)=1;
else
Cholnum(i)=10e10;    
end
end
Cholnum=Cholnum';

COPDnom=nominal(COPD);
getlevels(COPDnom)
COPDnum=[];
for i=1:length(COPD)
if COPDnom(i)=='no'
COPDnum(i)=0;
elseif COPDnom(i)=='yes'
COPDnum(i)=1;
else
COPDnum(i)=10e10;
end
end
COPDnum=COPDnum';

Alivenom=nominal(deadalive29102016);
getlevels(Alivenom)
Alivenum=[];
for i=1:length(deadalive29102016)
if Alivenom(i)=='alive'
Alivenum(i)=1;
elseif Alivenom(i)=='dead'
Alivenum(i)=0;
else
Alivenum(i)=10e10;    
end
end
Alivenum=Alivenum';

DeviceTherapynom=nominal(DeviceTherapy);
getlevels(DeviceTherapynom)
DeviceTherapynum=[];
for i=1:length(DeviceTherapy)
if DeviceTherapynom(i)=='no'
DeviceTherapynum(i)=0;
elseif DeviceTherapynom(i)=='yes'
DeviceTherapynum(i)=1;
else
DeviceTherapynum(i)=10e10;
end
end
DeviceTherapynum=DeviceTherapynum';

diabeticnom=nominal(diabetic);
getlevels(diabeticnom)
diabeticnum=[];
for i=1:length(diabetic)
if diabeticnom(i)=='no'
diabeticnum(i)=0;
elseif diabeticnom(i)=='yes'
diabeticnum(i)=1;
else
diabeticnum(i)=10e10;    
end
end
diabeticnum=diabeticnum';

ECGBlocknom=nominal(ECGBlock);
getlevels(ECGBlocknom)
ECGBlocknum=[];
for i=1:length(ECGBlock)
if ECGBlocknom(i)=='no'
ECGBlocknum(i)=0;
elseif ECGBlocknom(i)=='yes'
ECGBlocknum(i)=1;
else
ECGBlocknum(i)=10e10;    
end
end
ECGBlocknum=ECGBlocknum';

ECGBlockCnom=nominal(ECGblockComment);
getlevels(ECGBlockCnom)
ECGBlockCnum=[];
for i=1:length(ECGblockComment)
if ECGBlockCnom(i)=='normal'
ECGBlockCnum(i)=0;
elseif ECGBlockCnom(i)=='no'
ECGBlockCnum(i)=10e10;
else
ECGBlockCnum(i)=1;    
end
end
ECGBlockCnum=ECGBlockCnum';

ECGQRSnom=nominal(ECGQRS);
getlevels(ECGQRSnom)
ECGQRSnum=[];
for i=1:length(ECGQRS)
if ECGQRSnom(i)=='no'
ECGQRSnum(i)=0;
elseif ECGQRSnom(i)=='normal'
ECGQRSnum(i)=0;
elseif ECGQRSnom(i)=='LBBB'
ECGQRSnum(i)=1;
elseif ECGQRSnom(i)=='RBBB'
ECGQRSnum(i)=2;
elseif isundefined(ECGQRSnom(i))==1
ECGQRSnum(i)=10e10;  
else
ECGQRSnum(i)=3;      
end
end
ECGQRSnum=ECGQRSnum';
ECGQRSnum_old=ECGQRSnum;
ECGQRSnom_old=ECGQRSnom;
ECGQRSnom=nominal(ECGQRSnum);
ECGQRSlevels=getlevels(ECGQRSnom);
ECGQRSnum=zeros(length(ECGQRSnum),length(ECGQRSlevels));

for i=1:length(ECGQRSnum_old)
for j=1:length(ECGQRSlevels)
if ECGQRSnom(i)==ECGQRSlevels(j)
ECGQRSnum(i,j)=1;
end
end
end
ECGQRSnum=ECGQRSnum(:,1:end-1);
legends_ECGQRS={'normalECGQRS' 'LBBB' 'RBBB' 'ECGQRS_other'};

ECGRhythmnom=nominal(ECGRhythm);
getlevels(ECGRhythmnom)
ECGRhythmnum=[];
for i=1:length(ECGRhythm)
if ECGRhythmnom(i)=='AF'
ECGRhythmnum(i)=1;
elseif ECGRhythmnom(i)=='SR'
ECGRhythmnum(i)=2;
elseif isundefined(ECGRhythmnom(i))==1
ECGRhythmnum(i)=10e10;
else
ECGRhythmnum(i)=3;
end
end
ECGRhythmnum=ECGRhythmnum';
ECGRhythmnum_old=ECGRhythmnum;
ECGRhythmnom_old=ECGRhythmnom;
ECGRhythmnom=nominal(ECGRhythmnum);
ECGRhythmlevels=getlevels(ECGRhythmnom);
ECGRhythmnum=zeros(length(ECGRhythmnum),length(ECGRhythmlevels));

for i=1:length(ECGRhythmnum_old)
for j=1:length(ECGRhythmlevels)
if ECGRhythmnom(i)==ECGRhythmlevels(j)
ECGRhythmnum(i,j)=1;
end
end
end
ECGRhythmnum=ECGRhythmnum(:,1:end-1);
legends_ECGRhythm={'AF' 'SR' 'ECGRhythm_other'};

EthNicity_all=EthNicity;
EthNicity=char(EthNicity);
EthNicity=EthNicity(:,1);
EthNicitynom=nominal(EthNicity);
Etnlev=getlevels(EthNicitynom)
EthNicitynum=[];
for i=1:length(EthNicity)
if EthNicitynom(i)==Etnlev(1)
EthNicitynum(i)=1;
elseif EthNicitynom(i)==Etnlev(2)
EthNicitynum(i)=2;
elseif EthNicitynom(i)==Etnlev(3)
EthNicitynum(i)=3;
elseif EthNicitynom(i)==Etnlev(4)
EthNicitynum(i)=4;
elseif EthNicitynom(i)==Etnlev(5)
EthNicitynum(i)=5;
elseif EthNicitynom(i)==Etnlev(6)
EthNicitynum(i)=6;
elseif EthNicitynom(i)==Etnlev(7)
EthNicitynum(i)=7;
elseif EthNicitynom(i)==Etnlev(8)
EthNicitynum(i)=8;
elseif EthNicitynom(i)==Etnlev(9)
EthNicitynum(i)=9;
elseif EthNicitynom(i)==Etnlev(10)
EthNicitynum(i)=10;
elseif EthNicitynom(i)==Etnlev(11)
EthNicitynum(i)=11;
elseif EthNicitynom(i)==Etnlev(12)
EthNicitynum(i)=12;
elseif EthNicitynom(i)==Etnlev(13)
EthNicitynum(i)=13;
else
EthNicitynum(i)=10e10;
end
end
EthNicitynum=EthNicitynum';

Gendernom=nominal(Gender);
getlevels(Gendernom)
Gendernum=[];
for i=1:length(Gender)
if Gendernom(i)=='M'
Gendernum(i)=0;
elseif Gendernom(i)=='F'
Gendernum(i)=1;
else
Gendernum(i)=10e10;    
end
end
Gendernum=Gendernum';

Groupethnicitiesnom=nominal(Groupethnicities);
Etnlev2=getlevels(Groupethnicitiesnom)
Groupethnicitiesnum=[];
for i=1:length(Groupethnicities)
if Groupethnicitiesnom(i)==Etnlev2(1)
Groupethnicitiesnum(i)=1;
elseif Groupethnicitiesnom(i)==Etnlev2(2)
Groupethnicitiesnum(i)=2;
elseif Groupethnicitiesnom(i)==Etnlev2(3)
Groupethnicitiesnum(i)=10e10;
elseif Groupethnicitiesnom(i)==Etnlev2(4)
Groupethnicitiesnum(i)=4;
elseif Groupethnicitiesnom(i)==Etnlev2(5)
Groupethnicitiesnum(i)=5;
else
Groupethnicitiesnum(i)=10e10;    
end
end
Groupethnicitiesnum=Groupethnicitiesnum';
Groupethnicitiesnum_old=Groupethnicitiesnum;
Groupethnicitiesnom_old=Groupethnicitiesnom;
Groupethnicitiesnom=nominal(Groupethnicitiesnum);
Groupethnicitieslevels=getlevels(Groupethnicitiesnom);
Groupethnicitiesnum=zeros(length(Groupethnicitiesnum),length(Groupethnicitieslevels));

for i=1:length(Groupethnicitiesnum_old)
for j=1:length(Groupethnicitieslevels)
if Groupethnicitiesnom(i)==Groupethnicitieslevels(j)
Groupethnicitiesnum(i,j)=1;
end
end
end
Groupethnicitiesnum=Groupethnicitiesnum(:,1:end-1);
legends_Groupethnicities={'Asian' 'Black' 'Other_ethnicity' 'White'};

IHDnom=nominal(IHD);
getlevels(IHDnom)
IHDnum=[];
for i=1:length(IHD)
if IHDnom(i)=='no'
IHDnum(i)=0;
elseif IHDnom(i)=='yes'
IHDnum(i)=1;
else
IHDnum(i)=10e10;    
end
end
IHDnum=IHDnum';

Irondef=Irondeficientferr100ngmLor300anTSAT20;
Irondefnom=nominal(Irondef);
getlevels(Irondefnom)
Irondefnum=[];
for i=1:length(Irondef)
if Irondefnom(i)=='normal'
Irondefnum(i)=0;
elseif Irondefnom(i)=='iron deficient'
Irondefnum(i)=1;
else
Irondefnum(i)=10e10;    
end
end
Irondefnum=Irondefnum';

LVHnom=nominal(LVH);
getlevels(LVHnom)
LVHnum=[];
for i=1:length(LVH)
if LVHnom(i)=='no'
LVHnum(i)=0;
elseif LVHnom(i)=='yes'
LVHnum(i)=1;
else
LVHnum(i)=10e10;    
end
end
LVHnum=LVHnum';

LVHlev=LVHMild1315mod1618severe19;
LVHlevnom=nominal(LVHlev);
getlevels(LVHlevnom)
LVHlevnum=[];
for i=1:length(LVHlev)
if (LVHlevnom(i)=='no' | LVHlevnom(i)=='No')
LVHlevnum(i)=0;
elseif LVHlevnom(i)=='normal'
LVHlevnum(i)=0;
elseif (LVHlevnom(i)=='mild' | LVHlevnom(i)=='Mild' | LVHlevnom(i)=='Mild-Moderate')
LVHlevnum(i)=1;
elseif (LVHlevnom(i)=='moderate' | LVHlevnom(i)=='Moderate' | LVHlevnom(i)=='yes')
LVHlevnum(i)=2;
elseif LVHlevnom(i)=='severe'
LVHlevnum(i)=3;
else
LVHlevnum(i)=10e10;
end
end
LVHlevnum=LVHlevnum';


mRnom=nominal(MR);
getlevels(mRnom)
mRnum=[];
for i=1:length(MR)
if mRnom(i)=='no'
mRnum(i)=0;
elseif mRnom(i)=='normal'
mRnum(i)=0;
elseif (mRnom(i)=='mild' | mRnom(i)=='mld')
mRnum(i)=1;
elseif (mRnom(i)=='moderate' | mRnom(i)=='modertate')
mRnum(i)=2;
else
mRnum(i)=10e10;
end
end
mRnum=mRnum';

orthopnoeanom=nominal(orthopnoea);
getlevels(orthopnoeanom)
orthopnoeanum=[];
for i=1:length(orthopnoea)
if orthopnoeanom(i)=='no'
orthopnoeanum(i)=0;
elseif orthopnoeanom(i)=='yes'
orthopnoeanum(i)=1;
else
orthopnoeanum(i)=10e10;    
end
end
orthopnoeanum=orthopnoeanum';

OSAnom=nominal(OSA);
getlevels(OSAnom)
OSAnum=[];
for i=1:length(OSA)
if OSAnom(i)=='no'
OSAnum(i)=0;
elseif OSAnom(i)=='yes'
OSAnum(i)=1;
else
OSAnum(i)=10e10;    
end
end
OSAnum=OSAnum';

Othernom=nominal(Other);
Otherlev=getlevels(Othernom)
Othernum=[];
for i=1:length(Other)
if Othernom(i)==Otherlev(1)
Othernum(i)=1;
elseif Othernom(i)==Otherlev(2)
Othernum(i)=2;
elseif Othernom(i)==Otherlev(3)
Othernum(i)=3;
elseif Othernom(i)==Otherlev(4)
Othernum(i)=4;
elseif Othernom(i)==Otherlev(5)
Othernum(i)=5;
elseif Othernom(i)==Otherlev(6)
Othernum(i)=6;
elseif Othernom(i)==Otherlev(7)
Othernum(i)=7;
elseif Othernom(i)==Otherlev(8)
Othernum(i)=8;
else
Othernum(i)=10e10;
end
end
Othernum=Othernum';

otherillnessesnom=nominal(otherillnesses);
getlevels(otherillnessesnom)
otherillnessesnum=[];
for i=1:length(otherillnesses)
if otherillnessesnom(i)=='no'
otherillnessesnum(i)=0;
elseif otherillnessesnom(i)=='none'
otherillnessesnum(i)=0;
elseif isundefined(otherillnessesnom(i))==1
otherillnessesnum(i)=10e10;
else
otherillnessesnum(i)=1;    
end
end
otherillnessesnum=otherillnessesnum';

palpitationsdizzyfallsnom=nominal(palpitationsdizzyfalls);
getlevels(palpitationsdizzyfallsnom)
palpitationsdizzyfallsnum=[];
for i=1:length(palpitationsdizzyfalls)
if palpitationsdizzyfallsnom(i)=='no'
palpitationsdizzyfallsnum(i)=0;
elseif palpitationsdizzyfallsnom(i)=='yes'
palpitationsdizzyfallsnum(i)=1;
else
palpitationsdizzyfallsnum(i)=10e10;    
end
end
palpitationsdizzyfallsnum=palpitationsdizzyfallsnum';

Peripheraloedemanom=nominal(Peripheraloedema);
getlevels(Peripheraloedemanom)
Peripheraloedemanum=[];
for i=1:length(Peripheraloedema)
if Peripheraloedemanom(i)=='no'
Peripheraloedemanum(i)=0;
elseif Peripheraloedemanom(i)=='yes'
Peripheraloedemanum(i)=1;
else
Peripheraloedemanum(i)=10e10;    
end
end
Peripheraloedemanum=Peripheraloedemanum';

PnDnom=nominal(PND);
getlevels(PnDnom)
PnDnum=[];
for i=1:length(PND)
if PnDnom(i)=='no'
PnDnum(i)=0;
elseif PnDnom(i)=='yes'
PnDnum(i)=1;
else
PnDnum(i)=10e10;    
end
end
PnDnum=PnDnum';

RWmAnom=nominal(RWMA);
getlevels(RWmAnom)
RWmAnum=[];
for i=1:length(RWMA)
if RWmAnom(i)=='no'
RWmAnum(i)=0;
elseif RWmAnom(i)=='yes'
RWmAnum(i)=1;
else
RWmAnum(i)=10e10;    
end
end
RWmAnum=RWmAnum';

TWInom=nominal(TWI);
getlevels(TWInom)
TWInum=[];
for i=1:length(TWI)
if TWInom(i)=='no'
TWInum(i)=0;
elseif TWInom(i)=='yes'
TWInum(i)=1;
else
TWInum(i)=10e10;    
end
end
TWInum=TWInum';

RVfunctionnom=nominal(RVfunction);
getlevels(RVfunctionnom)
RVfunctionnum=[];
for i=1:length(RVfunction)
if RVfunctionnom(i)=='no'
RVfunctionnum(i)=0;
elseif (RVfunctionnom(i)=='normal' | RVfunctionnom(i)=='normL' | RVfunctionnom(i)=='Normal')
RVfunctionnum(i)=0;
elseif (RVfunctionnom(i)=='mild' | RVfunctionnom(i)=='Mild')
RVfunctionnum(i)=1;
elseif (RVfunctionnom(i)=='moderate' | RVfunctionnom(i)=='Moderate')
RVfunctionnum(i)=2;
elseif (RVfunctionnom(i)=='dilated' | RVfunctionnom(i)=='dilayed' | RVfunctionnom(i)=='dlated')
RVfunctionnum(i)=3;
elseif RVfunctionnom(i)=='impaired'
RVfunctionnum(i)=4;
else
RVfunctionnum(i)=10e10;
end
end
RVfunctionnum=RVfunctionnum';

TAPSE=TAPSEnormalSisover10normalTAPSEisover1;
TAPSEnom=nominal(TAPSE);
getlevels(TAPSEnom)
TAPSEnum=[];
for i=1:length(TAPSE)
if TAPSEnom(i)=='normal'
TAPSEnum(i)=0;
elseif TAPSEnom(i)=='abnormal'
TAPSEnum(i)=1;
else
TAPSEnum(i)=10e10;    
end
end
TAPSEnum=TAPSEnum';

TRnom=nominal(TR);
getlevels(TRnom)
TRnum=[];
for i=1:length(TR)
if TRnom(i)=='no'
TRnum(i)=0;
elseif TRnom(i)=='normal'
TRnum(i)=0;
elseif (TRnom(i)=='mild' | TRnom(i)=='Mild-Moderate')
TRnum(i)=1;
elseif (TRnom(i)=='moderate' | TRnom(i)=='Moderate')
TRnum(i)=2;
elseif TRnom(i)=='severe'
TRnum(i)=3;
else
TRnum(i)=10e10;
end
end
TRnum=TRnum';

DilatedLVnom=nominal(DilatedLV);
getlevels(DilatedLVnom)
DilatedLVnum=[];
for i=1:length(DilatedLV)
if (DilatedLVnom(i)=='no' | DilatedLVnom(i)=='normal')
DilatedLVnum(i)=0;
elseif DilatedLVnom(i)=='mild'
DilatedLVnum(i)=1;
else
DilatedLVnum(i)=10e10;    
end
end
DilatedLVnum=DilatedLVnum';

gradientnom=nominal(gradientmmHg);
getlevels(gradientnom)
gradientnum=[];
for i=1:length(gradientmmHg)
if gradientnom(i)=='no'
gradientnum(i)=0;
elseif gradientnom(i)=='yes'
gradientnum(i)=1;
else
gradientnum(i)=10e10;    
end
end
gradientnum=gradientnum';

Patient_group=CategoriesINinhosptdeathZdeadwithin30daysYdeadwithin1yearXdead1;
Patient_groupnom=nominal(Patient_group);
lev_pat=getlevels(Patient_groupnom)
Patient_groupnum=[];
for i=1:length(Patient_group)

if (Patient_groupnom(i)==lev_pat(3) | Patient_groupnom(i)==lev_pat(4)) 
    Patient_groupnum(i)=2;

elseif (Patient_groupnom(i)==lev_pat(8) | Patient_groupnom(i)==lev_pat(9) | Patient_groupnom(i)==lev_pat(10))
    Patient_groupnum(i)=3;

else
Patient_groupnum(i)=1;
end
end
Patient_groupnum=Patient_groupnum';

All_data=[Numberofcomorbiditied DM AF Age Albuminchecked Alivenum ARnum ASnum asymptomaticforHFnum Awave BP Breathlessnum calculatedEnormal11orover checkedadmissionweight checkedBMIadmissionBMImeansBMI30butnotsureofexactnumber checkedDBP checkedheight checkedpulse checkedSBP checkedweightchangesinceadmission Chestpainnum Chol COPDasthma DeviceTherapynum DilatedLVnum E_enormalunder8indeterminine813abnormal14orover EAimpairedunder08pseudo081normal115restrictive2 ECGBlocknum ECGBlockCnum ECGQRSnum ECGQRSduration ECGRate ECGRhythmnum Edeceltimeimpaired200restrictive160normalpseudo160200 Ewave Ferritinchecked followupfromBNPdays followupfromhospitaldischargedays Gendernum GFRchecked Glucosechecked Groupethnicitiesnum HBA1C Hbchecked IHD Irondefnum Ironlevelschecked Kchecked LAarea20upto20252030mod304040over40 LAdiameter43infemales47inmales LateralS LOS LVfunctionpreservedmeansover55 LVHlevnum LVHnum MAPcalculated MCVchecked mRnum Nachecked NTproBNPchecked NumberofadmissionsfromBNPuntil29102016 NYHAclassbreathless1notbreathlessto4verybreathless0notknown obesity obesityBMI30 orthopnoeanum OSA palpitationsdizzyfallsnum PASP PCVchecked Peripheraloedemanum Pltschecked PnDnum RAarea22upto22262230mod304040over40 RVfunctionnum RWmAnum TAPSEnum Timetonextadmission TimetoHFadmissionfrompreviousdischarge Timetoadmissionfrompreviousdischarge TRnum TSATchecked TWInum WBCchecked];
Varnames={'comorbidities' 'DM' 'AF' 'Age' 'Albumin' 'Alive' 'AR' 'AS' 'asymptHF' 'Awave' 'BP' 'Breathless' 'calculatedE' 'admissionwgt' 'BmIadmission' 'DBP' 'height' 'pulse' 'SBP' 'weightchange' 'Chestpain' 'Chol' 'COPD' 'DeviceTherapy' 'DilatedLV' 'E_e' 'EA' 'ECGBlock' 'ECGBlockComment' 'normalECGQRS' 'LBBB' 'RBBB' 'ECGQRS_other' 'ECGQRSduration' 'ECGRate' 'AF' 'SR' 'ECGRhythm_other' 'Edeceltime' 'Ewave' 'Ferritin' 'daysfollowupBnP' 'daysfollowupdischarge' 'Gender' 'GFR' 'Glucose' 'Asian' 'Black' 'Other_ethnicity' 'White' 'HBA1C' 'Hb' 'IHD' 'Irondef' 'Ironlevels' 'K' 'LAarea' 'LAdiameter' 'LateralS' 'LOS' 'LVfunction' 'LVHlev' 'LVH' 'MAP' 'MCV' 'MR' 'Na' 'NTproBNP' 'admissionsBNP' 'NYHAclass' 'obesity' 'obesityBMI30' 'orthopnoea' 'OSA' 'palpdizzyfalls' 'PASP' 'PCV' 'Peripheraloedema' 'Plt' 'PnD' 'RAarea' 'RVfunction' 'RWmA' 'TAPSE' 'Timetonextadm' 'TimetoHFadm' 'Timefromprevadm' 'TR' 'TSAT' 'TWI' 'WBC'};
nans=find(isnan(All_data)==1);

for i=1:size(All_data,2)
    n=find(All_data(:,i)==10e10);
    All_data(n,i)=All_data(nans(1));
end

symptomatic=find(All_data(:,9)==0);
All_data=All_data(symptomatic,:);
%Imputation
All_data_imp=All_data;
for i=1:size(All_data,2)
   impvar=All_data(:,i);
   j=find(isnan(impvar)==1);
   notimp=impvar(setdiff(1:length(impvar),j));
   All_data_imp(j,i)=mean(notimp);
end

%Finding a subset of the data without NaNs
Phenotypes=All_data_imp;
failed=[];
    for i=1:size(Phenotypes,1)
        f=find(isnan(Phenotypes(i,:))==1);
        g=find(isinf(Phenotypes(i,:))==1);
        h=find(sum(Phenotypes(i,:))==0);
        f=unique([f g h]);
        if isempty(f)==0
             failed=[failed;i];
        end
    end

    zero_var_ph=find(std(Phenotypes)<10e-2); %Removing phenotypes with zero variance prior to metamodelling
    use_ph=setdiff(1:size(Phenotypes,2),zero_var_ph);

All_data_imp=All_data_imp(:,use_ph);   
models={};    
predictions={}; 
options.PCs=20;
options.autoscaleX=1;
options.autoscaleY=1;
options.CV=0;
options.plot=0;
options.nrclusters=3;
options.matrix='SX';
options.classification='Fuzzy';
options.residuals=0;
options.reduced_clustering_rank=1;
options.secondorder=1;
options.sin_cos=0;
All_data_imp2=All_data_imp;
Varnames2=Varnames(use_ph);

for i=1:size(All_data_imp,2)
    Y=All_data_imp(:,i);
    X=All_data_imp(:,setdiff(1:size(All_data_imp,2),i));
    impvar=All_data(:,i);
    j=find(isnan(impvar)==1);
    
    testset=1:floor(size(X,1)/3);
    calset=setdiff(1:size(X,1),testset);
    
    hmod=HPLS(X(calset,:),Y(calset),options);
    pred=HPLSpred(X(testset,:),Y(testset),hmod);
    
    hmodall=HPLS(X,Y);
    predall=HPLSpred(X,[],hmodall);
    
    All_data_imp2(j,i)=predall.Ypredgr(j);
    models{i}=hmod;
    predictions{i}=pred;
end
    
R2=[];
for i=1:length(models)
    R2=[R2 predictions{i}.R2gr];
end

Catvar=[];
for i=1:size(All_data_imp2,2)
    if min(All_data_imp2(:,i))==0
        Catvar=[Catvar;i];
    end
end
Catvar=Catvar(2:end);

All_data_imp3=All_data_imp2;
All_data_imp3(:,Catvar)=round(All_data_imp3(:,Catvar));

%important in Varnames2:
important=[14 44 45 46 47 41 4 1 50 3 10 21 22 70 71 51 54 55 58 76 77 36 37 12 23 24 78 59 60 72 63 11 20 69 75 73 17 49 62 74 64 53 42 33 34 35];
important=sort(important);
All_data_imp3=All_data_imp3(:,important);

use_pat=find(All_data_imp3(:,65)>2000);
All_data_imp3=All_data_imp3(use_pat,:);
%Clustering
Y=Patient_groupnum(symptomatic,:);
Y=Y(use_pat,:);

Ynom=nominal(Y);
levelsY=getlevels(Ynom);

Y1=zeros(length(Y),length(levelsY));

for i=1:length(Y)
for j=1:length(levelsY)
if Ynom(i)==levelsY(j)
Y1(i,j)=1;
end
end
end
% 
%     load('weights_nov2016.mat')
%     Xuse = zscore(All_data_imp3);
%     Xuse=Xuse.*repmat(weights,size(Xuse,1),1);
%     [coeff, score, latent, tsquare] = princomp(Xuse);
   
[coeff, score, latent, tsquare] = princomp(zscore(All_data_imp3));
expl=cumsum(latent)./sum(latent);
usepcs=find(expl>0.99);
usepcs=usepcs(1);
Phenotypes=score(:,1:usepcs);

testclass=[];
for i=1:size(Phenotypes,1)
calset=setdiff(1:size(Phenotypes,1),i);
training=fitcknn(Phenotypes(calset,:),Y(calset));
testclass(i) = predict(training,Phenotypes(i,:));
end
error=abs(testclass'-Y);
error=find(error>0);
error=length(error)/length(testclass)*100;
correct=100-error


testclassknn=[];
for k=1:size(Y1,2)
for i=1:size(Y1,1)
calset=setdiff(1:size(Phenotypes,1),i);
training=fitcknn(Phenotypes(calset,:),Y1(calset,k));
testclassknn(i,k) = predict(training,Phenotypes(i,:));
end
end
error=sum(abs(testclassknn-Y1),2);
error=find(error>0);
error=length(error)/size(testclassknn,1)*100;
correct=100-error

more=find(sum(testclassknn,2)>1);
less=find(sum(testclassknn,2)<1);

testclass=[];
for k=1:size(Y1,2)
for i=1:size(Y1,1)
calset=setdiff(1:size(Phenotypes,1),i);
training=svmtrain(Phenotypes(calset,:),Y1(calset,k),'autoscale','true','kernel_function','polynomial');
testclass(i,k) = svmclassify(training,Phenotypes(i,:));
end
end
error=sum(abs(testclass-Y1),2);
error=find(error>0);
error=length(error)/size(testclass,1)*100;
correct=100-error

more=find(sum(testclass,2)>1);
less=find(sum(testclass,2)<1);

% i=find(isnan(Phenotypes)==1);
% Phenotypes(i)=0;
%zero_var_ph=find(std(Phenotypes)<10e-10);
%use_ph=setdiff(1:size(Phenotypes,2),zero_var_ph);
%Phenotypes=Phenotypes(:,use_ph);

% weights=ones(81,1);
% weights(1)=0;
% weights(15)=2;
% weights(5)=2;
% weights(22)=2;
% weights(39)=2;
% weights(29)=2;
% weights(43)=2;
% weights(80)=2;
% weights(67)=2;
% weights(30)=2;
% weights(31)=2;
% weights(7)=2;
% weights(19)=2;
% weights(14)=2;
% weights(20)=2;
% weights(37)=2;
% weights(81)=2;
% weights(76)=2;


options.PCs=20;
options.autoscaleX=1;
options.autoscaleY=1;
options.CV=0;
options.plot=0;
options.nrclusters=3;
options.matrix='SX';
options.classification='Fuzzy';
options.residuals=0;
options.reduced_clustering_rank=1;
options.secondorder=1;
options.sin_cos=0;


Phenotypes=All_data_imp3;
options.PCs=3;
testclass=[];
testclass2=[];
testclass3=[];
testclass4=[];
testclass5=[];
testclass6=[];
pred_phenos=[];
corrects=[];

Phenotypes=All_data_imp3;
for i=1:size(Y1,1)
    calset=setdiff(1:size(Phenotypes,1),i);
    %hmodcl=HPLS_supervised_clustering(Phenotypes(calset,:),Y1(calset,:),options,Y(calset));
    hmodcl=HPLS(Phenotypes(calset,:),Y1(calset,:),options);
    hpred=HPLSpred(Phenotypes(i,:),Y1(i,:),hmodcl);
    testclass(i,:)=hpred.Ypredgr;
    Phenotypes2=hmodcl.SX(:,1:3);
    pred_pheno2=hpred.SXnew;
    pred_phenos=[pred_phenos;pred_pheno2];
    training=fitcknn(Phenotypes2,Y(calset));
    testclass2(i) = predict(training,pred_pheno2(1:3));
    for k=1:size(Y1,2)
    training2=svmtrain(Phenotypes2,Y1(calset,k),'autoscale','true','kernel_function','polynomial');
    testclass3(i,k) = svmclassify(training2,pred_pheno2(1:3));
    end
    testclass4(i) = classify(pred_pheno2(1:3),Phenotypes2,Y(calset),'quadratic');
    
    
% % %     %Finds fuzzy means for the real classes for the calibration set, from how fm are calculated, and use this to classify the testset, to get the correct numbering of clusters for the classification.     
% % %     
% % %     %Fuzzy clustering
% % %     m=1;
% % %     %[U,J,fm,a,D] = fcm(SX(:,1:optPCs)*repmat((pctvar(2,1:optPCs))',1,optPCs),m,nrclusters);
% % %     fm = fuzzymeans(Y1(calset,:),Phenotypes2,m);
% % %    % [Uspec,Jspec,fmspec,aspec,Dspec] = fcm(spectra(calset,:),m,nrclusters_spec,Y2(calset,:));
% % % 
% % %     %[Umaxspec,classspec]=max(Uspec,[],2);
% % %     D2_cal = distancematrix(Phenotypes2,fm);   
% % %     U2_cal = oppdater(D2_cal,m);
% % %     [Umax2_cal,class2_cal]=max(U2_cal,[],2);
% % %     
% % %     D2 = distancematrix(pred_pheno2(1:3),fm);   
% % % 
% % % 
% % %     %Calculate U for new objects
% % %     U2 = oppdater(D2,m);
% % %     [Umax2,class2]=max(U2,[],2);
%     [IDX, C, SUMD, D] = kmeans(Phenotypes2, 4);
%     D2 = distancematrix(pred_pheno2(1:3),C');
%     [Umax2,class2]=min(D2,[],2);
%     testclass5(i)=class2;
training3=fitcdiscr(Phenotypes2,Y(calset),'DiscrimType','pseudoquadratic');
testclass5(i) = predict(training3,pred_pheno2(1:3));
%training3=fitcdiscr(Phenotypes(calset,:),Y(calset),'DiscrimType','diagquadratic');
%testclass5(i) = predict(training3,Phenotypes(i,:));

end

[maxcl,I]=max(testclass,[],2);
Ypred=I;
error=sum(abs(Ypred-Y),2);
error=find(error>0);
error=length(error)/size(Ypred,1)*100;
correct=100-error

error2=abs(testclass2'-Y);
error2=find(error2>0);
error2=length(error2)/length(testclass2)*100;
correct2=100-error2

more=find(sum(testclass2,2)>1);
less=find(sum(testclass2,2)<1);

error3=sum(abs(testclass3-Y1),2);
error3=find(error3>0);
error3=length(error3)/size(testclass3,1)*100;
correct3=100-error3

more2=find(sum(testclass3,2)>1);
less2=find(sum(testclass3,2)<1);

error4=abs(testclass4'-Y);
error4=find(error4>0);
error4=length(error4)/length(testclass4)*100;
correct4=100-error4

error5=abs(testclass5'-Y);
error5=find(error5>0);
error5=length(error5)/length(testclass5)*100;
correct5=100-error5

[coeff, score, latent, tsquare] = princomp(zscore(All_data_imp3));
%     Xuse = zscore(All_data_imp3);
%     Xuse=Xuse.*repmat(weights,size(Xuse,1),1);
%     [coeff, score, latent, tsquare] = princomp(Xuse);
expl=cumsum(latent)./sum(latent);
for perc=[0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 0.99]
usepcs=find(expl>perc);
usepcs=usepcs(1);
Phenotypes=score(:,1:usepcs);
for i=1:size(Y1,1)
    calset=setdiff(1:size(Phenotypes,1),i);

    training3=fitcdiscr(Phenotypes(calset,:),Y(calset),'DiscrimType','diagquadratic');
    testclass6(i) = predict(training3,Phenotypes(i,:));

end

error6=abs(testclass6'-Y);
error6=find(error6>0);
error6=length(error6)/length(testclass6)*100;
correct6=100-error6

corrects=[corrects;correct6];
end

%Relative errors in groups
Err=abs(testclass6'-Y);
Err=find(Err>0);
Err=Y2(Err);

cl1=find(Err==1);
cl2=find(Err==2);
cl3=find(Err==3);
cl4=find(Err==4);
cl5=find(Err==5);
cl6=find(Err==6);

cl12=find(Y2==1);
cl22=find(Y2==2);
cl32=find(Y2==3);
cl42=find(Y2==4);
cl52=find(Y2==5);
cl62=find(Y2==6);
relnrerrors=[length(cl1)/length(cl12) length(cl2)/length(cl22) length(cl3)/length(cl32) length(cl4)/length(cl42) length(cl5)/length(cl52) length(cl6)/length(cl62)];
relnrerrors=relnrerrors*100;
relnrerrors



%%%
Phenotypes=All_data_imp3;
options.PCs=20;
hmodcl=HPLS(Phenotypes,Y1,options);
T=hmodcl.SX;
cl1=find(Y==1);
cl2=find(Y==2);
cl3=find(Y==3);
cl4=find(Y==4);
%cl5=find(Y==5);
%cl6=find(Y==6);
% cl1=find(hmodcl.clusters==1);
% cl2=find(hmodcl.clusters==2);
% cl3=find(hmodcl.clusters==3);
% cl4=find(hmodcl.clusters==4);
%cl5=find(hmodcl.clusters==5);
%cl6=find(hmodcl.clusters==6);
plot3(T(cl1,1),T(cl1,2),T(cl1,3),'*')
hold
plot3(T(cl2,1),T(cl2,2),T(cl2,3),'r*')
plot3(T(cl3,1),T(cl3,2),T(cl3,3),'g*')
plot3(T(cl4,1),T(cl4,2),T(cl4,3),'c*')
%plot3(T(cl5,1),T(cl5,2),T(cl5,3),'k*')
%plot3(T(cl6,1),T(cl6,2),T(cl6,3),'y*')
%legend('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Cluster 5','Cluster 6')
legend('Cluster 1','Cluster 2','Cluster 3','Cluster 4')
xlabel('Principal Component 1')
ylabel('Principal Component 2')
zlabel('Principal Component 3')
axis tight
ID=1:246;
for i=1:length(ID)
    text(T(i,1),T(i,2),T(i,3),num2str(ID(i)))
end
cl1=find(IDX==1);
cl2=find(IDX==2);
cl3=find(IDX==3);
cl4=find(IDX==4);

for i=1:length(Y)
if Y(i)==1
Ykmeans(i)=2;
elseif Y(i)==2
Ykmeans(i)=3;
elseif Y(i)==3
Ykmeans(i)=4;
elseif Y(i)==4
Ykmeans(i)=1;
end
end

Patient_groupnom=nominal(Patient_group);
getlevels(Patient_groupnom)
Patient_groupnum=[];
for i=1:length(Patient_group)
if Patient_groupnom(i)=='A'
    Patient_groupnum(i)=1;
elseif Patient_groupnom(i)=='B'
    %Patient_groupnum(i)=1;
Patient_groupnum(i)=2;
elseif Patient_groupnom(i)=='C'
    %Patient_groupnum(i)=2;
Patient_groupnum(i)=3;
elseif Patient_groupnom(i)=='D'
    %Patient_groupnum(i)=3;
Patient_groupnum(i)=4;
elseif Patient_groupnom(i)=='E'
    %Patient_groupnum(i)=4;
Patient_groupnum(i)=5;
elseif Patient_groupnom(i)=='F'
    %Patient_groupnum(i)=4;
Patient_groupnum(i)=6;
else
Patient_groupnum(i)=10e10;
end
end
Patient_groupnum=Patient_groupnum';

Y2=Patient_groupnum;

YF=[];
for i=1:length(Y2)
if Y2(i)==6
YF(i)=1;
else
YF(i)=0;
end
end

figure
H=subplot(3,1,1);
bar(hmodcl.LX(1:length(varnames),1))
ax = gca;
ax.XTick = [1:length(varnames)];
ax.XTickLabels = varnames;
ax.XTickLabelRotation = 90;
axis tight
ylabel('PC1 loadings')
H=subplot(3,1,2);
bar(hmodcl.LX(1:length(varnames),2))
ax = gca;
ax.XTick = [1:length(varnames)];
ax.XTickLabels = varnames;
ax.XTickLabelRotation = 90;
axis tight
ylabel('PC2 loadings')
H=subplot(3,1,3);
bar(hmodcl.LX(1:length(varnames),3))
ax = gca;
ax.XTick = [1:length(varnames)];
ax.XTickLabels = varnames;
ax.XTickLabelRotation = 90;
axis tight
ylabel('PC3 loadings')