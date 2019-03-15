#### Notes: Investing.R .............................................. ####

# This script describes a protocol for selecting files from a folder of
# csv data downloaded automatically from Investing.com, and
# converting these files to the xts format for further analysis in R.

# Specifically these data were collected using the webscraper, Investing.py.

# To perform this process, first state the location on your local
# machine where you have stored the csv data.

# Mine is shown below, as an example of the file path syntax in R.
# See line 24. This location can be changed for your unique set up.

# We then define the function for performing the conversion, and convert
# each file within this script.

# The reason for scripting this process as opposed to using a function
# is that in order to complete the process on each file, we need to
# convert each file to the "daily" format so they will be more easily
# recognized by xts based functions. This is more easily done with a script.

#### Investing.com folder ............................................ ####
Investing.com<-("C:\\Users\\Frankie\\Desktop\\Investing.com")

#### Convert to xts: Syntax: Investing.csv.xts(SPX) .................. ####
Investing.csv.xts<-function(..., type = "p"){
  
  # Require quantmod in order to work with xts
  require(quantmod)
  
  name<-deparse(substitute(...))
  data<-data.frame(...)
  
  # Defining the set of dates.
  Dates<-as.character(data[,1])
  Dates<-as.character(gsub(",","",Dates))
  Dates<-as.Date(Dates, format = "%b %d %Y")
  
  # Type: Price ("p") or yield ("y")
  if(type == "p"){
    n = 1
  }
  if(type == "y"){
    n = 100
  }
  
  # Reformatting the data.
  if((colnames(data)[6])=="Vol.") {
    # Reformatting the data.
    # Open
    data[,3]<-as.character(data[,3])
    data[,3]<-as.numeric(gsub(",","",data[,3]))*(1/n)
    # High
    data[,4]<-as.character(data[,4])
    data[,4]<-as.numeric(gsub(",","",data[,4]))*(1/n)
    # Low
    data[,5]<-as.character(data[,5])
    data[,5]<-as.numeric(gsub(",","",data[,5]))*(1/n)
    # Close
    data[,2]<-as.character(data[,2])
    data[,2]<-as.numeric(gsub(",","",data[,2]))*(1/n)
    # Volume 
    data[,6]<-as.character(data[,6])
    
    # Unit function for converting abbreviated units
    Unit<-function(x){
      
      if((grepl("-", x))=="TRUE"){
        x<-as.numeric(0)
      }
      
      if((grepl("K", x))=="TRUE"){
        x<-strsplit(x, "K")
        x<-as.numeric(x[1])
        x<-x*1000
      }
      
      if((grepl("M", x))=="TRUE"){
        x<-strsplit(x, "M")
        x<-as.numeric(x[1])
        x<-x*1000000
      }
      
      if((grepl("B", x))=="TRUE"){
        x<-strsplit(x, "B")
        x<-as.numeric(x[1])
        x<-x*1000000000
      }
      
      return(
        x
      )
    }
    
    data[,6]<-as.numeric(sapply(data[,6],Unit))
    
    # Defining the data frame.
    if(sum(data[,6])==0) {
      # Defining the data frame.
      data<-data.frame(cbind(data[,3], data[,4], data[,5], data[,2]))
      colnames(data)[1]<-paste(name,"Open", sep=".")
      colnames(data)[2]<-paste(name,"High", sep=".")
      colnames(data)[3]<-paste(name,"Low", sep=".")
      colnames(data)[4]<-paste(name,"Close", sep=".")
      row.names(data)<-Dates
      
    } else {
      # Defining the data frame.
      data<-data.frame(cbind(data[,3], data[,4], data[,5], data[,2], data[,6]))
      colnames(data)[1]<-paste(name,"Open", sep=".")
      colnames(data)[2]<-paste(name,"High", sep=".")
      colnames(data)[3]<-paste(name,"Low", sep=".")
      colnames(data)[4]<-paste(name,"Close", sep=".")
      colnames(data)[5]<-paste(name,"Volume", sep=".")
      row.names(data)<-Dates
    }
  } else {
    # Reformatting the data.
    # Open
    data[,3]<-as.character(data[,3])
    data[,3]<-as.numeric(gsub(",","",data[,3]))*(1/n)
    # High
    data[,4]<-as.character(data[,4])
    data[,4]<-as.numeric(gsub(",","",data[,4]))*(1/n)
    # Low
    data[,5]<-as.character(data[,5])
    data[,5]<-as.numeric(gsub(",","",data[,5]))*(1/n)
    # Close
    data[,2]<-as.character(data[,2])
    data[,2]<-as.numeric(gsub(",","",data[,2]))*(1/n)
    
    # Defining the data frame.
    data<-data.frame(cbind(data[,3], data[,4], data[,5], data[,2]))
    colnames(data)[1]<-paste(name,"Open", sep=".")
    colnames(data)[2]<-paste(name,"High", sep=".")
    colnames(data)[3]<-paste(name,"Low", sep=".")
    colnames(data)[4]<-paste(name,"Close", sep=".")
    row.names(data)<-Dates
  }
  
  # Assigning the data to the environment.
  return(
    assign(name, (data<-as.xts(data)), env=parent.frame())
  )
}

#### Convert all index data .......................................... ####

# DJIA
DJIA<-data.frame(read.csv(file.path(Investing.com,"DJIA.csv")))
Investing.csv.xts(DJIA)
DJIA<-to.daily(DJIA)

# SPX
SPX<-data.frame(read.csv(file.path(Investing.com,"SPX.csv")))
Investing.csv.xts(SPX)
SPX<-to.daily(SPX)

# IXIC
IXIC<-data.frame(read.csv(file.path(Investing.com,"IXIC.csv")))
Investing.csv.xts(IXIC)
IXIC<-to.daily(IXIC)

# RUT
RUT<-data.frame(read.csv(file.path(Investing.com,"RUT.csv")))
Investing.csv.xts(RUT)
RUT<-to.daily(RUT)

# VIX
VIX<-data.frame(read.csv(file.path(Investing.com,"VIX.csv")))
Investing.csv.xts(VIX)
VIX<-to.daily(VIX)

# GSPTSE
GSPTSE<-data.frame(read.csv(file.path(Investing.com,"GSPTSE.csv")))
Investing.csv.xts(GSPTSE)
GSPTSE<-to.daily(GSPTSE)

# BVSP
BVSP<-data.frame(read.csv(file.path(Investing.com,"BVSP.csv")))
Investing.csv.xts(BVSP)
BVSP<-to.daily(BVSP)

# MXX
MXX<-data.frame(read.csv(file.path(Investing.com,"MXX.csv")))
Investing.csv.xts(MXX)
MXX<-to.daily(MXX)

# GDAXI
GDAXI<-data.frame(read.csv(file.path(Investing.com,"GDAXI.csv")))
Investing.csv.xts(GDAXI)
GDAXI<-to.daily(GDAXI)

# FTSE
FTSE<-data.frame(read.csv(file.path(Investing.com,"FTSE.csv")))
Investing.csv.xts(FTSE)
FTSE<-to.daily(FTSE)

# FCHI
FCHI<-data.frame(read.csv(file.path(Investing.com,"FCHI.csv")))
Investing.csv.xts(FCHI)
FCHI<-to.daily(FCHI)

# STOXX50E
STOXX50E<-data.frame(read.csv(file.path(Investing.com,"STOXX50E.csv")))
Investing.csv.xts(STOXX50E)
STOXX50E<-to.daily(STOXX50E)

# AEX
AEX<-data.frame(read.csv(file.path(Investing.com,"AEX.csv")))
Investing.csv.xts(AEX)
AEX<-to.daily(AEX)

# IBEX
IBEX<-data.frame(read.csv(file.path(Investing.com,"IBEX.csv")))
Investing.csv.xts(IBEX)
IBEX<-to.daily(IBEX)

# FTSEMIB
FTSEMIB<-data.frame(read.csv(file.path(Investing.com,"FTSEMIB.csv")))
Investing.csv.xts(FTSEMIB)
FTSEMIB<-to.daily(FTSEMIB)

# SSMI
SSMI<-data.frame(read.csv(file.path(Investing.com,"SSMI.csv")))
Investing.csv.xts(SSMI)
SSMI<-to.daily(SSMI)

# PSI20
PSI20<-data.frame(read.csv(file.path(Investing.com,"PSI20.csv")))
Investing.csv.xts(PSI20)
PSI20<-to.daily(PSI20)

# BFX
BFX<-data.frame(read.csv(file.path(Investing.com,"BFX.csv")))
Investing.csv.xts(BFX)
BFX<-to.daily(BFX)

# ATX
ATX<-data.frame(read.csv(file.path(Investing.com,"ATX.csv")))
Investing.csv.xts(ATX)
ATX<-to.daily(ATX)

# OMX
OMX<-data.frame(read.csv(file.path(Investing.com,"OMX.csv")))
Investing.csv.xts(OMX)
OMX<-to.daily(OMX)

# OMXC25
OMXC25<-data.frame(read.csv(file.path(Investing.com,"OMXC25.csv")))
Investing.csv.xts(OMXC25)
OMXC25<-to.daily(OMXC25)

# MOEX
MOEX<-data.frame(read.csv(file.path(Investing.com,"MOEX.csv")))
Investing.csv.xts(MOEX)
MOEX<-to.daily(MOEX)

# RTSI
RTSI<-data.frame(read.csv(file.path(Investing.com,"RTSI.csv")))
Investing.csv.xts(RTSI)
RTSI<-to.daily(RTSI)

# WIG20
WIG20<-data.frame(read.csv(file.path(Investing.com,"WIG20.csv")))
Investing.csv.xts(WIG20)
WIG20<-to.daily(WIG20)

# BUX
BUX<-data.frame(read.csv(file.path(Investing.com,"BUX.csv")))
Investing.csv.xts(BUX)
BUX<-to.daily(BUX)

# XU100
XU100<-data.frame(read.csv(file.path(Investing.com,"XU100.csv")))
Investing.csv.xts(XU100)
XU100<-to.daily(XU100)

# TA35
TA35<-data.frame(read.csv(file.path(Investing.com,"TA35.csv")))
Investing.csv.xts(TA35)
TA35<-to.daily(TA35)

# SASEIDX
SASEIDX<-data.frame(read.csv(file.path(Investing.com,"SASEIDX.csv")))
Investing.csv.xts(SASEIDX)
SASEIDX<-to.daily(SASEIDX)

# NKY
NKY<-data.frame(read.csv(file.path(Investing.com,"NKY.csv")))
Investing.csv.xts(NKY)
NKY<-to.daily(NKY)

# AS51
AS51<-data.frame(read.csv(file.path(Investing.com,"AS51.csv")))
Investing.csv.xts(AS51)
AS51<-to.daily(AS51)

# NZDOW
NZDOW<-data.frame(read.csv(file.path(Investing.com,"NZDOW.csv")))
Investing.csv.xts(NZDOW)
NZDOW<-to.daily(NZDOW)

# SHCOMP
SHCOMP<-data.frame(read.csv(file.path(Investing.com,"SHCOMP.csv")))
Investing.csv.xts(SHCOMP)
SHCOMP<-to.daily(SHCOMP)

# SICOM
SICOM<-data.frame(read.csv(file.path(Investing.com,"SICOM.csv")))
Investing.csv.xts(SICOM)
SICOM<-to.daily(SICOM)

# TXIN9
TXIN9<-data.frame(read.csv(file.path(Investing.com,"TXIN9.csv")))
Investing.csv.xts(TXIN9)
TXIN9<-to.daily(TXIN9)

# DJSH
DJSH<-data.frame(read.csv(file.path(Investing.com,"DJSH.csv")))
Investing.csv.xts(DJSH)
DJSH<-to.daily(DJSH)

# HSI
HSI<-data.frame(read.csv(file.path(Investing.com,"HSI.csv")))
Investing.csv.xts(HSI)
HSI<-to.daily(HSI)

# TWSE
TWSE<-data.frame(read.csv(file.path(Investing.com,"TWSE.csv")))
Investing.csv.xts(TWSE)
TWSE<-to.daily(TWSE)

# SET
SET<-data.frame(read.csv(file.path(Investing.com,"SET.csv")))
Investing.csv.xts(SET)
SET<-to.daily(SET)

# KOSPI
KOSPI<-data.frame(read.csv(file.path(Investing.com,"KOSPI.csv")))
Investing.csv.xts(KOSPI)
KOSPI<-to.daily(KOSPI)

# JCI
JCI<-data.frame(read.csv(file.path(Investing.com,"JCI.csv")))
Investing.csv.xts(JCI)
JCI<-to.daily(JCI)

# NIFTY
NIFTY<-data.frame(read.csv(file.path(Investing.com,"NIFTY.csv")))
Investing.csv.xts(NIFTY)
NIFTY<-to.daily(NIFTY)

# SENSEX
SENSEX<-data.frame(read.csv(file.path(Investing.com,"SENSEX.csv")))
Investing.csv.xts(SENSEX)
SENSEX<-to.daily(SENSEX)

# PCOMP
PCOMP<-data.frame(read.csv(file.path(Investing.com,"PCOMP.csv")))
Investing.csv.xts(PCOMP)
PCOMP<-to.daily(PCOMP)

# FSSTI
FSSTI<-data.frame(read.csv(file.path(Investing.com,"FSSTI.csv")))
Investing.csv.xts(FSSTI)
FSSTI<-to.daily(FSSTI)

# KSE100
KSE100<-data.frame(read.csv(file.path(Investing.com,"KSE100.csv")))
Investing.csv.xts(KSE100)
KSE100<-to.daily(KSE100)

# HNX30
HNX30<-data.frame(read.csv(file.path(Investing.com,"HNX30.csv")))
Investing.csv.xts(HNX30)
HNX30<-to.daily(HNX30)

# CSEALL
CSEALL<-data.frame(read.csv(file.path(Investing.com,"CSEALL.csv")))
Investing.csv.xts(CSEALL)
CSEALL<-to.daily(CSEALL)

#### Convert Watchlist ............................................... ####
LYB<-data.frame(read.csv(file.path(Investing.com,"LYB.csv")))
Investing.csv.xts(LYB)
LYB<-to.daily(LYB)

PPG<-data.frame(read.csv(file.path(Investing.com,"PPG.csv")))
Investing.csv.xts(PPG)
PPG<-to.daily(PPG)

SHW<-data.frame(read.csv(file.path(Investing.com,"SHW.csv")))
Investing.csv.xts(SHW)
SHW<-to.daily(SHW)

IFF<-data.frame(read.csv(file.path(Investing.com,"IFF.csv")))
Investing.csv.xts(IFF)
IFF<-to.daily(IFF)

DWDP<-data.frame(read.csv(file.path(Investing.com,"DWDP.csv")))
Investing.csv.xts(DWDP)
DWDP<-to.daily(DWDP)

PX<-data.frame(read.csv(file.path(Investing.com,"PX.csv")))
Investing.csv.xts(PX)
PX<-to.daily(PX)

APD<-data.frame(read.csv(file.path(Investing.com,"APD.csv")))
Investing.csv.xts(APD)
APD<-to.daily(APD)

EMN<-data.frame(read.csv(file.path(Investing.com,"EMN.csv")))
Investing.csv.xts(EMN)
EMN<-to.daily(EMN)

FMC<-data.frame(read.csv(file.path(Investing.com,"FMC.csv")))
Investing.csv.xts(FMC)
FMC<-to.daily(FMC)

CF<-data.frame(read.csv(file.path(Investing.com,"CF.csv")))
Investing.csv.xts(CF)
CF<-to.daily(CF)

JNJ<-data.frame(read.csv(file.path(Investing.com,"JNJ.csv")))
Investing.csv.xts(JNJ)
JNJ<-to.daily(JNJ)

PFE<-data.frame(read.csv(file.path(Investing.com,"PFE.csv")))
Investing.csv.xts(PFE)
PFE<-to.daily(PFE)

MRK<-data.frame(read.csv(file.path(Investing.com,"MRK.csv")))
Investing.csv.xts(MRK)
MRK<-to.daily(MRK)

ABBV<-data.frame(read.csv(file.path(Investing.com,"ABBV.csv")))
Investing.csv.xts(ABBV)
ABBV<-to.daily(ABBV)

BMY<-data.frame(read.csv(file.path(Investing.com,"BMY.csv")))
Investing.csv.xts(BMY)
BMY<-to.daily(BMY)

LLY<-data.frame(read.csv(file.path(Investing.com,"LLY.csv")))
Investing.csv.xts(LLY)
LLY<-to.daily(LLY)

UNH<-data.frame(read.csv(file.path(Investing.com,"UNH.csv")))
Investing.csv.xts(UNH)
UNH<-to.daily(UNH)

CVS<-data.frame(read.csv(file.path(Investing.com,"CVS.csv")))
Investing.csv.xts(CVS)
CVS<-to.daily(CVS)

ESRX<-data.frame(read.csv(file.path(Investing.com,"ESRX.csv")))
Investing.csv.xts(ESRX)
ESRX<-to.daily(ESRX)

AET<-data.frame(read.csv(file.path(Investing.com,"AET.csv")))
Investing.csv.xts(AET)
AET<-to.daily(AET)

ANTM<-data.frame(read.csv(file.path(Investing.com,"ANTM.csv")))
Investing.csv.xts(ANTM)
ANTM<-to.daily(ANTM)

CI<-data.frame(read.csv(file.path(Investing.com,"CI.csv")))
Investing.csv.xts(CI)
CI<-to.daily(CI)

HUM<-data.frame(read.csv(file.path(Investing.com,"HUM.csv")))
Investing.csv.xts(HUM)
HUM<-to.daily(HUM)

GILD<-data.frame(read.csv(file.path(Investing.com,"GILD.csv")))
Investing.csv.xts(GILD)
GILD<-to.daily(GILD)

AMGN<-data.frame(read.csv(file.path(Investing.com,"AMGN.csv")))
Investing.csv.xts(AMGN)
AMGN<-to.daily(AMGN)

CELG<-data.frame(read.csv(file.path(Investing.com,"CELG.csv")))
Investing.csv.xts(CELG)
CELG<-to.daily(CELG)

BIIB<-data.frame(read.csv(file.path(Investing.com,"BIIB.csv")))
Investing.csv.xts(BIIB)
BIIB<-to.daily(BIIB)

REGN<-data.frame(read.csv(file.path(Investing.com,"REGN.csv")))
Investing.csv.xts(REGN)
REGN<-to.daily(REGN)

ALXN<-data.frame(read.csv(file.path(Investing.com,"ALXN.csv")))
Investing.csv.xts(ALXN)
ALXN<-to.daily(ALXN)

VRTX<-data.frame(read.csv(file.path(Investing.com,"VRTX.csv")))
Investing.csv.xts(VRTX)
VRTX<-to.daily(VRTX)

MDT<-data.frame(read.csv(file.path(Investing.com,"MDT.csv")))
Investing.csv.xts(MDT)
MDT<-to.daily(MDT)

ABT<-data.frame(read.csv(file.path(Investing.com,"ABT.csv")))
Investing.csv.xts(ABT)
ABT<-to.daily(ABT)

SYK<-data.frame(read.csv(file.path(Investing.com,"SYK.csv")))
Investing.csv.xts(SYK)
SYK<-to.daily(SYK)

BSX<-data.frame(read.csv(file.path(Investing.com,"BSX.csv")))
Investing.csv.xts(BSX)
BSX<-to.daily(BSX)

ZBH<-data.frame(read.csv(file.path(Investing.com,"ZBH.csv")))
Investing.csv.xts(ZBH)
ZBH<-to.daily(ZBH)

ISRG<-data.frame(read.csv(file.path(Investing.com,"ISRG.csv")))
Investing.csv.xts(ISRG)
ISRG<-to.daily(ISRG)

EW<-data.frame(read.csv(file.path(Investing.com,"EW.csv")))
Investing.csv.xts(EW)
EW<-to.daily(EW)

VAR<-data.frame(read.csv(file.path(Investing.com,"VAR.csv")))
Investing.csv.xts(VAR)
VAR<-to.daily(VAR)

TMO<-data.frame(read.csv(file.path(Investing.com,"TMO.csv")))
Investing.csv.xts(TMO)
TMO<-to.daily(TMO)

A<-data.frame(read.csv(file.path(Investing.com,"A.csv")))
Investing.csv.xts(A)
A<-to.daily(A)

LH<-data.frame(read.csv(file.path(Investing.com,"LH.csv")))
Investing.csv.xts(LH)
LH<-to.daily(LH)

DGX<-data.frame(read.csv(file.path(Investing.com,"DGX.csv")))
Investing.csv.xts(DGX)
DGX<-to.daily(DGX)

PKI<-data.frame(read.csv(file.path(Investing.com,"PKI.csv")))
Investing.csv.xts(PKI)
PKI<-to.daily(PKI)

BDX<-data.frame(read.csv(file.path(Investing.com,"BDX.csv")))
Investing.csv.xts(BDX)
BDX<-to.daily(BDX)

BAX<-data.frame(read.csv(file.path(Investing.com,"BAX.csv")))
Investing.csv.xts(BAX)
BAX<-to.daily(BAX)

WAT<-data.frame(read.csv(file.path(Investing.com,"WAT.csv")))
Investing.csv.xts(WAT)
WAT<-to.daily(WAT)

XRAY<-data.frame(read.csv(file.path(Investing.com,"XRAY.csv")))
Investing.csv.xts(XRAY)
XRAY<-to.daily(XRAY)

HCA<-data.frame(read.csv(file.path(Investing.com,"HCA.csv")))
Investing.csv.xts(HCA)
HCA<-to.daily(HCA)

UHS<-data.frame(read.csv(file.path(Investing.com,"UHS.csv")))
Investing.csv.xts(UHS)
UHS<-to.daily(UHS)

THC<-data.frame(read.csv(file.path(Investing.com,"THC.csv")))
Investing.csv.xts(THC)
THC<-to.daily(THC)

ENDP<-data.frame(read.csv(file.path(Investing.com,"ENDP.csv")))
Investing.csv.xts(ENDP)
ENDP<-to.daily(ENDP)

DVA<-data.frame(read.csv(file.path(Investing.com,"DVA.csv")))
Investing.csv.xts(DVA)
DVA<-to.daily(DVA)

PRGO<-data.frame(read.csv(file.path(Investing.com,"PRGO.csv")))
Investing.csv.xts(PRGO)
PRGO<-to.daily(PRGO)

AGN<-data.frame(read.csv(file.path(Investing.com,"AGN.csv")))
Investing.csv.xts(AGN)
AGN<-to.daily(AGN)

MYL<-data.frame(read.csv(file.path(Investing.com,"MYL.csv")))
Investing.csv.xts(MYL)
MYL<-to.daily(MYL)

ZTS<-data.frame(read.csv(file.path(Investing.com,"ZTS.csv")))
Investing.csv.xts(ZTS)
ZTS<-to.daily(ZTS)

MNK<-data.frame(read.csv(file.path(Investing.com,"MNK.csv")))
Investing.csv.xts(MNK)
MNK<-to.daily(MNK)

AI.PA<-data.frame(read.csv(file.path(Investing.com,"AI.PA.csv")))
Investing.csv.xts(AI.PA)
AI.PA<-to.daily(AI.PA)

BNP.PA<-data.frame(read.csv(file.path(Investing.com,"BNP.PA.csv")))
Investing.csv.xts(BNP.PA)
BNP.PA<-to.daily(BNP.PA)

ACA.PA<-data.frame(read.csv(file.path(Investing.com,"ACA.PA.csv")))
Investing.csv.xts(ACA.PA)
ACA.PA<-to.daily(ACA.PA)

SAN.PA<-data.frame(read.csv(file.path(Investing.com,"SAN.PA.csv")))
Investing.csv.xts(SAN.PA)
SAN.PA<-to.daily(SAN.PA)

GLE.PA<-data.frame(read.csv(file.path(Investing.com,"GLE.PA.csv")))
Investing.csv.xts(GLE.PA)
GLE.PA<-to.daily(GLE.PA)

SOLB.BR<-data.frame(read.csv(file.path(Investing.com,"SOLB.BR.csv")))
Investing.csv.xts(SOLB.BR)
SOLB.BR<-to.daily(SOLB.BR)

FTI.PA<-data.frame(read.csv(file.path(Investing.com,"FTI.PA.csv")))
Investing.csv.xts(FTI.PA)
FTI.PA<-to.daily(FTI.PA)

FP.PA<-data.frame(read.csv(file.path(Investing.com,"FP.PA.csv")))
Investing.csv.xts(FP.PA)
FP.PA<-to.daily(FP.PA)

#### Convert all bond data ........................................... ####

EGYOvernight<-data.frame(read.csv(file.path(Investing.com,"EGYOvernight.csv")))
Investing.csv.xts(EGYOvernight, type = "y")
EGYOvernight<-to.daily(EGYOvernight)

KENOvernight<-data.frame(read.csv(file.path(Investing.com,"KENOvernight.csv")))
Investing.csv.xts(KENOvernight, type = "y")
KENOvernight<-to.daily(KENOvernight)

MEXOvernight<-data.frame(read.csv(file.path(Investing.com,"MEXOvernight.csv")))
Investing.csv.xts(MEXOvernight, type = "y")
MEXOvernight<-to.daily(MEXOvernight)

POLOvernight<-data.frame(read.csv(file.path(Investing.com,"POLOvernight.csv")))
Investing.csv.xts(POLOvernight, type = "y")
POLOvernight<-to.daily(POLOvernight)

RUSOvernight<-data.frame(read.csv(file.path(Investing.com,"RUSOvernight.csv")))
Investing.csv.xts(RUSOvernight, type = "y")
RUSOvernight<-to.daily(RUSOvernight)

CHEOvernight<-data.frame(read.csv(file.path(Investing.com,"CHEOvernight.csv")))
Investing.csv.xts(CHEOvernight, type = "y")
CHEOvernight<-to.daily(CHEOvernight)

HKG1W<-data.frame(read.csv(file.path(Investing.com,"HKG1W.csv")))
Investing.csv.xts(HKG1W, type = "y")
HKG1W<-to.daily(HKG1W)

RUS1W<-data.frame(read.csv(file.path(Investing.com,"RUS1W.csv")))
Investing.csv.xts(RUS1W, type = "y")
RUS1W<-to.daily(RUS1W)

CHE1W<-data.frame(read.csv(file.path(Investing.com,"CHE1W.csv")))
Investing.csv.xts(CHE1W, type = "y")
CHE1W<-to.daily(CHE1W)

RUS2W<-data.frame(read.csv(file.path(Investing.com,"RUS2W.csv")))
Investing.csv.xts(RUS2W, type = "y")
RUS2W<-to.daily(RUS2W)

MYS3W<-data.frame(read.csv(file.path(Investing.com,"MYS3W.csv")))
Investing.csv.xts(MYS3W, type = "y")
MYS3W<-to.daily(MYS3W)

BEL1M<-data.frame(read.csv(file.path(Investing.com,"BEL1M.csv")))
Investing.csv.xts(BEL1M, type = "y")
BEL1M<-to.daily(BEL1M)

BGR1M<-data.frame(read.csv(file.path(Investing.com,"BGR1M.csv")))
Investing.csv.xts(BGR1M, type = "y")
BGR1M<-to.daily(BGR1M)

CAN1M<-data.frame(read.csv(file.path(Investing.com,"CAN1M.csv")))
Investing.csv.xts(CAN1M, type = "y")
CAN1M<-to.daily(CAN1M)

CHL1M<-data.frame(read.csv(file.path(Investing.com,"CHL1M.csv")))
Investing.csv.xts(CHL1M, type = "y")
CHL1M<-to.daily(CHL1M)

FRA1M<-data.frame(read.csv(file.path(Investing.com,"FRA1M.csv")))
Investing.csv.xts(FRA1M, type = "y")
FRA1M<-to.daily(FRA1M)

GRC1M<-data.frame(read.csv(file.path(Investing.com,"GRC1M.csv")))
Investing.csv.xts(GRC1M, type = "y")
GRC1M<-to.daily(GRC1M)

HKG1M<-data.frame(read.csv(file.path(Investing.com,"HKG1M.csv")))
Investing.csv.xts(HKG1M, type = "y")
HKG1M<-to.daily(HKG1M)

IDN1M<-data.frame(read.csv(file.path(Investing.com,"IDN1M.csv")))
Investing.csv.xts(IDN1M, type = "y")
IDN1M<-to.daily(IDN1M)

ISR1M<-data.frame(read.csv(file.path(Investing.com,"ISR1M.csv")))
Investing.csv.xts(ISR1M, type = "y")
ISR1M<-to.daily(ISR1M)

ITA1M<-data.frame(read.csv(file.path(Investing.com,"ITA1M.csv")))
Investing.csv.xts(ITA1M, type = "y")
ITA1M<-to.daily(ITA1M)

JPN1M<-data.frame(read.csv(file.path(Investing.com,"JPN1M.csv")))
Investing.csv.xts(JPN1M, type = "y")
JPN1M<-to.daily(JPN1M)

MLT1M<-data.frame(read.csv(file.path(Investing.com,"MLT1M.csv")))
Investing.csv.xts(MLT1M, type = "y")
MLT1M<-to.daily(MLT1M)

MEX1M<-data.frame(read.csv(file.path(Investing.com,"MEX1M.csv")))
Investing.csv.xts(MEX1M, type = "y")
MEX1M<-to.daily(MEX1M)

NLD1M<-data.frame(read.csv(file.path(Investing.com,"NLD1M.csv")))
Investing.csv.xts(NLD1M, type = "y")
NLD1M<-to.daily(NLD1M)

NZL1M<-data.frame(read.csv(file.path(Investing.com,"NZL1M.csv")))
Investing.csv.xts(NZL1M, type = "y")
NZL1M<-to.daily(NZL1M)

NOR1M<-data.frame(read.csv(file.path(Investing.com,"NOR1M.csv")))
Investing.csv.xts(NOR1M, type = "y")
NOR1M<-to.daily(NOR1M)

PHL1M<-data.frame(read.csv(file.path(Investing.com,"PHL1M.csv")))
Investing.csv.xts(PHL1M, type = "y")
PHL1M<-to.daily(PHL1M)

POL1M<-data.frame(read.csv(file.path(Investing.com,"POL1M.csv")))
Investing.csv.xts(POL1M, type = "y")
POL1M<-to.daily(POL1M)

RUS1M<-data.frame(read.csv(file.path(Investing.com,"RUS1M.csv")))
Investing.csv.xts(RUS1M, type = "y")
RUS1M<-to.daily(RUS1M)

SGP1M<-data.frame(read.csv(file.path(Investing.com,"SGP1M.csv")))
Investing.csv.xts(SGP1M, type = "y")
SGP1M<-to.daily(SGP1M)

ESP1M<-data.frame(read.csv(file.path(Investing.com,"ESP1M.csv")))
Investing.csv.xts(ESP1M, type = "y")
ESP1M<-to.daily(ESP1M)

SWE1M<-data.frame(read.csv(file.path(Investing.com,"SWE1M.csv")))
Investing.csv.xts(SWE1M, type = "y")
SWE1M<-to.daily(SWE1M)

CHE1M<-data.frame(read.csv(file.path(Investing.com,"CHE1M.csv")))
Investing.csv.xts(CHE1M, type = "y")
CHE1M<-to.daily(CHE1M)

GBR1M<-data.frame(read.csv(file.path(Investing.com,"GBR1M.csv")))
Investing.csv.xts(GBR1M, type = "y")
GBR1M<-to.daily(GBR1M)

USA1M<-data.frame(read.csv(file.path(Investing.com,"USA1M.csv")))
Investing.csv.xts(USA1M, type = "y")
USA1M<-to.daily(USA1M)

CAN2M<-data.frame(read.csv(file.path(Investing.com,"CAN2M.csv")))
Investing.csv.xts(CAN2M, type = "y")
CAN2M<-to.daily(CAN2M)

ITA3M<-data.frame(read.csv(file.path(Investing.com,"ITA3M.csv")))
Investing.csv.xts(ITA3M, type = "y")
ITA3M<-to.daily(ITA3M)

MUS2M<-data.frame(read.csv(file.path(Investing.com,"MUS2M.csv")))
Investing.csv.xts(MUS2M, type = "y")
MUS2M<-to.daily(MUS2M)

NZL2M<-data.frame(read.csv(file.path(Investing.com,"NZL2M.csv")))
Investing.csv.xts(NZL2M, type = "y")
NZL2M<-to.daily(NZL2M)

NOR2M<-data.frame(read.csv(file.path(Investing.com,"NOR2M.csv")))
Investing.csv.xts(NOR2M, type = "y")
NOR2M<-to.daily(NOR2M)

POL2M<-data.frame(read.csv(file.path(Investing.com,"POL2M.csv")))
Investing.csv.xts(POL2M, type = "y")
POL2M<-to.daily(POL2M)

RUS2M<-data.frame(read.csv(file.path(Investing.com,"RUS2M.csv")))
Investing.csv.xts(RUS2M, type = "y")
RUS2M<-to.daily(RUS2M)

SWE2M<-data.frame(read.csv(file.path(Investing.com,"SWE2M.csv")))
Investing.csv.xts(SWE2M, type = "y")
SWE2M<-to.daily(SWE2M)

CHE2M<-data.frame(read.csv(file.path(Investing.com,"CHE2M.csv")))
Investing.csv.xts(CHE2M, type = "y")
CHE2M<-to.daily(CHE2M)

BHR3M<-data.frame(read.csv(file.path(Investing.com,"BHR3M.csv")))
Investing.csv.xts(BHR3M, type = "y")
BHR3M<-to.daily(BHR3M)

BGD3M<-data.frame(read.csv(file.path(Investing.com,"BGD3M.csv")))
Investing.csv.xts(BGD3M, type = "y")
BGD3M<-to.daily(BGD3M)

BEL3M<-data.frame(read.csv(file.path(Investing.com,"BEL3M.csv")))
Investing.csv.xts(BEL3M, type = "y")
BEL3M<-to.daily(BEL3M)

BRA3M<-data.frame(read.csv(file.path(Investing.com,"BRA3M.csv")))
Investing.csv.xts(BRA3M, type = "y")
BRA3M<-to.daily(BRA3M)

CAN3M<-data.frame(read.csv(file.path(Investing.com,"CAN3M.csv")))
Investing.csv.xts(CAN3M, type = "y")
CAN3M<-to.daily(CAN3M)

DNK3M<-data.frame(read.csv(file.path(Investing.com,"DNK3M.csv")))
Investing.csv.xts(DNK3M, type = "y")
DNK3M<-to.daily(DNK3M)

EGY3M<-data.frame(read.csv(file.path(Investing.com,"EGY3M.csv")))
Investing.csv.xts(EGY3M, type = "y")
EGY3M<-to.daily(EGY3M)

FRA3M<-data.frame(read.csv(file.path(Investing.com,"FRA3M.csv")))
Investing.csv.xts(FRA3M, type = "y")
FRA3M<-to.daily(FRA3M)

DEU3M<-data.frame(read.csv(file.path(Investing.com,"DEU3M.csv")))
Investing.csv.xts(DEU3M, type = "y")
DEU3M<-to.daily(DEU3M)

GRC3M<-data.frame(read.csv(file.path(Investing.com,"GRC3M.csv")))
Investing.csv.xts(GRC3M, type = "y")
GRC3M<-to.daily(GRC3M)

HKG3M<-data.frame(read.csv(file.path(Investing.com,"HKG3M.csv")))
Investing.csv.xts(HKG3M, type = "y")
HKG3M<-to.daily(HKG3M)

HUN3M<-data.frame(read.csv(file.path(Investing.com,"HUN3M.csv")))
Investing.csv.xts(HUN3M, type = "y")
HUN3M<-to.daily(HUN3M)

IND3M<-data.frame(read.csv(file.path(Investing.com,"IND3M.csv")))
Investing.csv.xts(IND3M, type = "y")
IND3M<-to.daily(IND3M)

IDN3M<-data.frame(read.csv(file.path(Investing.com,"IDN3M.csv")))
Investing.csv.xts(IDN3M, type = "y")
IDN3M<-to.daily(IDN3M)

IRL3M<-data.frame(read.csv(file.path(Investing.com,"IRL3M.csv")))
Investing.csv.xts(IRL3M, type = "y")
IRL3M<-to.daily(IRL3M)

ISR3M<-data.frame(read.csv(file.path(Investing.com,"ISR3M.csv")))
Investing.csv.xts(ISR3M, type = "y")
ISR3M<-to.daily(ISR3M)

JPN3M<-data.frame(read.csv(file.path(Investing.com,"JPN3M.csv")))
Investing.csv.xts(JPN3M, type = "y")
JPN3M<-to.daily(JPN3M)

JOR3M<-data.frame(read.csv(file.path(Investing.com,"JOR3M.csv")))
Investing.csv.xts(JOR3M, type = "y")
JOR3M<-to.daily(JOR3M)

KEN3M<-data.frame(read.csv(file.path(Investing.com,"KEN3M.csv")))
Investing.csv.xts(KEN3M, type = "y")
KEN3M<-to.daily(KEN3M)

MYS3M<-data.frame(read.csv(file.path(Investing.com,"MYS3M.csv")))
Investing.csv.xts(MYS3M, type = "y")
MYS3M<-to.daily(MYS3M)

MLT3M<-data.frame(read.csv(file.path(Investing.com,"MLT3M.csv")))
Investing.csv.xts(MLT3M, type = "y")
MLT3M<-to.daily(MLT3M)

MEX3M<-data.frame(read.csv(file.path(Investing.com,"MEX3M.csv")))
Investing.csv.xts(MEX3M, type = "y")
MEX3M<-to.daily(MEX3M)

MAR3M<-data.frame(read.csv(file.path(Investing.com,"MAR3M.csv")))
Investing.csv.xts(MAR3M, type = "y")
MAR3M<-to.daily(MAR3M)

NAM3M<-data.frame(read.csv(file.path(Investing.com,"NAM3M.csv")))
Investing.csv.xts(NAM3M, type = "y")
NAM3M<-to.daily(NAM3M)

NLD3M<-data.frame(read.csv(file.path(Investing.com,"NLD3M.csv")))
Investing.csv.xts(NLD3M, type = "y")
NLD3M<-to.daily(NLD3M)

NZL3M<-data.frame(read.csv(file.path(Investing.com,"NZL3M.csv")))
Investing.csv.xts(NZL3M, type = "y")
NZL3M<-to.daily(NZL3M)

NGA3M<-data.frame(read.csv(file.path(Investing.com,"NGA3M.csv")))
Investing.csv.xts(NGA3M, type = "y")
NGA3M<-to.daily(NGA3M)

NOR3M<-data.frame(read.csv(file.path(Investing.com,"NOR3M.csv")))
Investing.csv.xts(NOR3M, type = "y")
NOR3M<-to.daily(NOR3M)

PAK3M<-data.frame(read.csv(file.path(Investing.com,"PAK3M.csv")))
Investing.csv.xts(PAK3M, type = "y")
PAK3M<-to.daily(PAK3M)

PHL3M<-data.frame(read.csv(file.path(Investing.com,"PHL3M.csv")))
Investing.csv.xts(PHL3M, type = "y")
PHL3M<-to.daily(PHL3M)

PRT3M<-data.frame(read.csv(file.path(Investing.com,"PRT3M.csv")))
Investing.csv.xts(PRT3M, type = "y")
PRT3M<-to.daily(PRT3M)

RUS3M<-data.frame(read.csv(file.path(Investing.com,"RUS3M.csv")))
Investing.csv.xts(RUS3M, type = "y")
RUS3M<-to.daily(RUS3M)

SGP3M<-data.frame(read.csv(file.path(Investing.com,"SGP3M.csv")))
Investing.csv.xts(SGP3M, type = "y")
SGP3M<-to.daily(SGP3M)

ZAF3M<-data.frame(read.csv(file.path(Investing.com,"ZAF3M.csv")))
Investing.csv.xts(ZAF3M, type = "y")
ZAF3M<-to.daily(ZAF3M)

ESP3M<-data.frame(read.csv(file.path(Investing.com,"ESP3M.csv")))
Investing.csv.xts(ESP3M, type = "y")
ESP3M<-to.daily(ESP3M)

LKA3M<-data.frame(read.csv(file.path(Investing.com,"LKA3M.csv")))
Investing.csv.xts(LKA3M, type = "y")
LKA3M<-to.daily(LKA3M)

SWE3M<-data.frame(read.csv(file.path(Investing.com,"SWE3M.csv")))
Investing.csv.xts(SWE3M, type = "y")
SWE3M<-to.daily(SWE3M)

CHE3M<-data.frame(read.csv(file.path(Investing.com,"CHE3M.csv")))
Investing.csv.xts(CHE3M, type = "y")
CHE3M<-to.daily(CHE3M)

UGA3M<-data.frame(read.csv(file.path(Investing.com,"UGA3M.csv")))
Investing.csv.xts(UGA3M, type = "y")
UGA3M<-to.daily(UGA3M)

GBR3M<-data.frame(read.csv(file.path(Investing.com,"GBR3M.csv")))
Investing.csv.xts(GBR3M, type = "y")
GBR3M<-to.daily(GBR3M)

USA3M<-data.frame(read.csv(file.path(Investing.com,"USA3M.csv")))
Investing.csv.xts(USA3M, type = "y")
USA3M<-to.daily(USA3M)

MUS4M<-data.frame(read.csv(file.path(Investing.com,"MUS4M.csv")))
Investing.csv.xts(MUS4M, type = "y")
MUS4M<-to.daily(MUS4M)

NZL4M<-data.frame(read.csv(file.path(Investing.com,"NZL4M.csv")))
Investing.csv.xts(NZL4M, type = "y")
NZL4M<-to.daily(NZL4M)

NZL5M<-data.frame(read.csv(file.path(Investing.com,"NZL5M.csv")))
Investing.csv.xts(NZL5M, type = "y")
NZL5M<-to.daily(NZL5M)

ITA6M<-data.frame(read.csv(file.path(Investing.com,"ITA6M.csv")))
Investing.csv.xts(ITA6M, type = "y")
ITA6M<-to.daily(ITA6M)

BHR6M<-data.frame(read.csv(file.path(Investing.com,"BHR6M.csv")))
Investing.csv.xts(BHR6M, type = "y")
BHR6M<-to.daily(BHR6M)

BGD6M<-data.frame(read.csv(file.path(Investing.com,"BGD6M.csv")))
Investing.csv.xts(BGD6M, type = "y")
BGD6M<-to.daily(BGD6M)

BEL6M<-data.frame(read.csv(file.path(Investing.com,"BEL6M.csv")))
Investing.csv.xts(BEL6M, type = "y")
BEL6M<-to.daily(BEL6M)

BWA6M<-data.frame(read.csv(file.path(Investing.com,"BWA6M.csv")))
Investing.csv.xts(BWA6M, type = "y")
BWA6M<-to.daily(BWA6M)

BRA6M<-data.frame(read.csv(file.path(Investing.com,"BRA6M.csv")))
Investing.csv.xts(BRA6M, type = "y")
BRA6M<-to.daily(BRA6M)

CAN6M<-data.frame(read.csv(file.path(Investing.com,"CAN6M.csv")))
Investing.csv.xts(CAN6M, type = "y")
CAN6M<-to.daily(CAN6M)

HRV6M<-data.frame(read.csv(file.path(Investing.com,"HRV6M.csv")))
Investing.csv.xts(HRV6M, type = "y")
HRV6M<-to.daily(HRV6M)

DNK6M<-data.frame(read.csv(file.path(Investing.com,"DNK6M.csv")))
Investing.csv.xts(DNK6M, type = "y")
DNK6M<-to.daily(DNK6M)

EGY6M<-data.frame(read.csv(file.path(Investing.com,"EGY6M.csv")))
Investing.csv.xts(EGY6M, type = "y")
EGY6M<-to.daily(EGY6M)

FRA6M<-data.frame(read.csv(file.path(Investing.com,"FRA6M.csv")))
Investing.csv.xts(FRA6M, type = "y")
FRA6M<-to.daily(FRA6M)

DEU6M<-data.frame(read.csv(file.path(Investing.com,"DEU6M.csv")))
Investing.csv.xts(DEU6M, type = "y")
DEU6M<-to.daily(DEU6M)

GRC6M<-data.frame(read.csv(file.path(Investing.com,"GRC6M.csv")))
Investing.csv.xts(GRC6M, type = "y")
GRC6M<-to.daily(GRC6M)

HKG6M<-data.frame(read.csv(file.path(Investing.com,"HKG6M.csv")))
Investing.csv.xts(HKG6M, type = "y")
HKG6M<-to.daily(HKG6M)

HUN6M<-data.frame(read.csv(file.path(Investing.com,"HUN6M.csv")))
Investing.csv.xts(HUN6M, type = "y")
HUN6M<-to.daily(HUN6M)

IND6M<-data.frame(read.csv(file.path(Investing.com,"IND6M.csv")))
Investing.csv.xts(IND6M, type = "y")
IND6M<-to.daily(IND6M)

IDN6M<-data.frame(read.csv(file.path(Investing.com,"IDN6M.csv")))
Investing.csv.xts(IDN6M, type = "y")
IDN6M<-to.daily(IDN6M)

IRL6M<-data.frame(read.csv(file.path(Investing.com,"IRL6M.csv")))
Investing.csv.xts(IRL6M, type = "y")
IRL6M<-to.daily(IRL6M)

ISR6M<-data.frame(read.csv(file.path(Investing.com,"ISR6M.csv")))
Investing.csv.xts(ISR6M, type = "y")
ISR6M<-to.daily(ISR6M)

JPN6M<-data.frame(read.csv(file.path(Investing.com,"JPN6M.csv")))
Investing.csv.xts(JPN6M, type = "y")
JPN6M<-to.daily(JPN6M)

JOR6M<-data.frame(read.csv(file.path(Investing.com,"JOR6M.csv")))
Investing.csv.xts(JOR6M, type = "y")
JOR6M<-to.daily(JOR6M)

KEN6M<-data.frame(read.csv(file.path(Investing.com,"KEN6M.csv")))
Investing.csv.xts(KEN6M, type = "y")
KEN6M<-to.daily(KEN6M)

MLT6M<-data.frame(read.csv(file.path(Investing.com,"MLT6M.csv")))
Investing.csv.xts(MLT6M, type = "y")
MLT6M<-to.daily(MLT6M)

MUS6M<-data.frame(read.csv(file.path(Investing.com,"MUS6M.csv")))
Investing.csv.xts(MUS6M, type = "y")
MUS6M<-to.daily(MUS6M)

MEX6M<-data.frame(read.csv(file.path(Investing.com,"MEX6M.csv")))
Investing.csv.xts(MEX6M, type = "y")
MEX6M<-to.daily(MEX6M)

MAR6M<-data.frame(read.csv(file.path(Investing.com,"MAR6M.csv")))
Investing.csv.xts(MAR6M, type = "y")
MAR6M<-to.daily(MAR6M)

NAM6M<-data.frame(read.csv(file.path(Investing.com,"NAM6M.csv")))
Investing.csv.xts(NAM6M, type = "y")
NAM6M<-to.daily(NAM6M)

NLD6M<-data.frame(read.csv(file.path(Investing.com,"NLD6M.csv")))
Investing.csv.xts(NLD6M, type = "y")
NLD6M<-to.daily(NLD6M)

NZL6M<-data.frame(read.csv(file.path(Investing.com,"NZL6M.csv")))
Investing.csv.xts(NZL6M, type = "y")
NZL6M<-to.daily(NZL6M)

NGA6M<-data.frame(read.csv(file.path(Investing.com,"NGA6M.csv")))
Investing.csv.xts(NGA6M, type = "y")
NGA6M<-to.daily(NGA6M)

NOR6M<-data.frame(read.csv(file.path(Investing.com,"NOR6M.csv")))
Investing.csv.xts(NOR6M, type = "y")
NOR6M<-to.daily(NOR6M)

PAK6M<-data.frame(read.csv(file.path(Investing.com,"PAK6M.csv")))
Investing.csv.xts(PAK6M, type = "y")
PAK6M<-to.daily(PAK6M)

PHL6M<-data.frame(read.csv(file.path(Investing.com,"PHL6M.csv")))
Investing.csv.xts(PHL6M, type = "y")
PHL6M<-to.daily(PHL6M)

PRT6M<-data.frame(read.csv(file.path(Investing.com,"PRT6M.csv")))
Investing.csv.xts(PRT6M, type = "y")
PRT6M<-to.daily(PRT6M)

ROU6M<-data.frame(read.csv(file.path(Investing.com,"ROU6M.csv")))
Investing.csv.xts(ROU6M, type = "y")
ROU6M<-to.daily(ROU6M)

RUS6M<-data.frame(read.csv(file.path(Investing.com,"RUS6M.csv")))
Investing.csv.xts(RUS6M, type = "y")
RUS6M<-to.daily(RUS6M)

SGP6M<-data.frame(read.csv(file.path(Investing.com,"SGP6M.csv")))
Investing.csv.xts(SGP6M, type = "y")
SGP6M<-to.daily(SGP6M)

ESP6M<-data.frame(read.csv(file.path(Investing.com,"ESP6M.csv")))
Investing.csv.xts(ESP6M, type = "y")
ESP6M<-to.daily(ESP6M)

LKA6M<-data.frame(read.csv(file.path(Investing.com,"LKA6M.csv")))
Investing.csv.xts(LKA6M, type = "y")
LKA6M<-to.daily(LKA6M)

SWE6M<-data.frame(read.csv(file.path(Investing.com,"SWE6M.csv")))
Investing.csv.xts(SWE6M, type = "y")
SWE6M<-to.daily(SWE6M)

CHE6M<-data.frame(read.csv(file.path(Investing.com,"CHE6M.csv")))
Investing.csv.xts(CHE6M, type = "y")
CHE6M<-to.daily(CHE6M)

UGA6M<-data.frame(read.csv(file.path(Investing.com,"UGA6M.csv")))
Investing.csv.xts(UGA6M, type = "y")
UGA6M<-to.daily(UGA6M)

GBR6M<-data.frame(read.csv(file.path(Investing.com,"GBR6M.csv")))
Investing.csv.xts(GBR6M, type = "y")
GBR6M<-to.daily(GBR6M)

USA6M<-data.frame(read.csv(file.path(Investing.com,"USA6M.csv")))
Investing.csv.xts(USA6M, type = "y")
USA6M<-to.daily(USA6M)

MYS7M<-data.frame(read.csv(file.path(Investing.com,"MYS7M.csv")))
Investing.csv.xts(MYS7M, type = "y")
MYS7M<-to.daily(MYS7M)

MUS8M<-data.frame(read.csv(file.path(Investing.com,"MUS8M.csv")))
Investing.csv.xts(MUS8M, type = "y")
MUS8M<-to.daily(MUS8M)

ITA9M<-data.frame(read.csv(file.path(Investing.com,"ITA9M.csv")))
Investing.csv.xts(ITA9M, type = "y")
ITA9M<-to.daily(ITA9M)

BHR9M<-data.frame(read.csv(file.path(Investing.com,"BHR9M.csv")))
Investing.csv.xts(BHR9M, type = "y")
BHR9M<-to.daily(BHR9M)

BEL9M<-data.frame(read.csv(file.path(Investing.com,"BEL9M.csv")))
Investing.csv.xts(BEL9M, type = "y")
BEL9M<-to.daily(BEL9M)

BRA9M<-data.frame(read.csv(file.path(Investing.com,"BRA9M.csv")))
Investing.csv.xts(BRA9M, type = "y")
BRA9M<-to.daily(BRA9M)

HRV9M<-data.frame(read.csv(file.path(Investing.com,"HRV9M.csv")))
Investing.csv.xts(HRV9M, type = "y")
HRV9M<-to.daily(HRV9M)

EGY9M<-data.frame(read.csv(file.path(Investing.com,"EGY9M.csv")))
Investing.csv.xts(EGY9M, type = "y")
EGY9M<-to.daily(EGY9M)

FRA9M<-data.frame(read.csv(file.path(Investing.com,"FRA9M.csv")))
Investing.csv.xts(FRA9M, type = "y")
FRA9M<-to.daily(FRA9M)

DEU9M<-data.frame(read.csv(file.path(Investing.com,"DEU9M.csv")))
Investing.csv.xts(DEU9M, type = "y")
DEU9M<-to.daily(DEU9M)

HKG9M<-data.frame(read.csv(file.path(Investing.com,"HKG9M.csv")))
Investing.csv.xts(HKG9M, type = "y")
HKG9M<-to.daily(HKG9M)

ISR9M<-data.frame(read.csv(file.path(Investing.com,"ISR9M.csv")))
Investing.csv.xts(ISR9M, type = "y")
ISR9M<-to.daily(ISR9M)

IDN1Y<-data.frame(read.csv(file.path(Investing.com,"IDN1Y.csv")))
Investing.csv.xts(IDN1Y, type = "y")
IDN1Y<-to.daily(IDN1Y)

JPN9M<-data.frame(read.csv(file.path(Investing.com,"JPN9M.csv")))
Investing.csv.xts(JPN9M, type = "y")
JPN9M<-to.daily(JPN9M)

JOR9M<-data.frame(read.csv(file.path(Investing.com,"JOR9M.csv")))
Investing.csv.xts(JOR9M, type = "y")
JOR9M<-to.daily(JOR9M)

MEX9M<-data.frame(read.csv(file.path(Investing.com,"MEX9M.csv")))
Investing.csv.xts(MEX9M, type = "y")
MEX9M<-to.daily(MEX9M)

NAM9M<-data.frame(read.csv(file.path(Investing.com,"NAM9M.csv")))
Investing.csv.xts(NAM9M, type = "y")
NAM9M<-to.daily(NAM9M)

NOR9M<-data.frame(read.csv(file.path(Investing.com,"NOR9M.csv")))
Investing.csv.xts(NOR9M, type = "y")
NOR9M<-to.daily(NOR9M)

ESP9M<-data.frame(read.csv(file.path(Investing.com,"ESP9M.csv")))
Investing.csv.xts(ESP9M, type = "y")
ESP9M<-to.daily(ESP9M)

ARG1Y<-data.frame(read.csv(file.path(Investing.com,"ARG1Y.csv")))
Investing.csv.xts(ARG1Y, type = "y")
ARG1Y<-to.daily(ARG1Y)

AUS1Y<-data.frame(read.csv(file.path(Investing.com,"AUS1Y.csv")))
Investing.csv.xts(AUS1Y, type = "y")
AUS1Y<-to.daily(AUS1Y)

AUT1Y<-data.frame(read.csv(file.path(Investing.com,"AUT1Y.csv")))
Investing.csv.xts(AUT1Y, type = "y")
AUT1Y<-to.daily(AUT1Y)

BHR1Y<-data.frame(read.csv(file.path(Investing.com,"BHR1Y.csv")))
Investing.csv.xts(BHR1Y, type = "y")
BHR1Y<-to.daily(BHR1Y)

BGD1Y<-data.frame(read.csv(file.path(Investing.com,"BGD1Y.csv")))
Investing.csv.xts(BGD1Y, type = "y")
BGD1Y<-to.daily(BGD1Y)

BEL1Y<-data.frame(read.csv(file.path(Investing.com,"BEL1Y.csv")))
Investing.csv.xts(BEL1Y, type = "y")
BEL1Y<-to.daily(BEL1Y)

BRA1Y<-data.frame(read.csv(file.path(Investing.com,"BRA1Y.csv")))
Investing.csv.xts(BRA1Y, type = "y")
BRA1Y<-to.daily(BRA1Y)

BGR1Y<-data.frame(read.csv(file.path(Investing.com,"BGR1Y.csv")))
Investing.csv.xts(BGR1Y, type = "y")
BGR1Y<-to.daily(BGR1Y)

CAN1Y<-data.frame(read.csv(file.path(Investing.com,"CAN1Y.csv")))
Investing.csv.xts(CAN1Y, type = "y")
CAN1Y<-to.daily(CAN1Y)

CHL1Y<-data.frame(read.csv(file.path(Investing.com,"CHL1Y.csv")))
Investing.csv.xts(CHL1Y, type = "y")
CHL1Y<-to.daily(CHL1Y)

CHN1Y<-data.frame(read.csv(file.path(Investing.com,"CHN1Y.csv")))
Investing.csv.xts(CHN1Y, type = "y")
CHN1Y<-to.daily(CHN1Y)

COL1Y<-data.frame(read.csv(file.path(Investing.com,"COL1Y.csv")))
Investing.csv.xts(COL1Y, type = "y")
COL1Y<-to.daily(COL1Y)

HRV1Y<-data.frame(read.csv(file.path(Investing.com,"HRV1Y.csv")))
Investing.csv.xts(HRV1Y, type = "y")
HRV1Y<-to.daily(HRV1Y)

CZE1Y<-data.frame(read.csv(file.path(Investing.com,"CZE1Y.csv")))
Investing.csv.xts(CZE1Y, type = "y")
CZE1Y<-to.daily(CZE1Y)

EGY1Y<-data.frame(read.csv(file.path(Investing.com,"EGY1Y.csv")))
Investing.csv.xts(EGY1Y, type = "y")
EGY1Y<-to.daily(EGY1Y)

FRA1Y<-data.frame(read.csv(file.path(Investing.com,"FRA1Y.csv")))
Investing.csv.xts(FRA1Y, type = "y")
FRA1Y<-to.daily(FRA1Y)

DEU1Y<-data.frame(read.csv(file.path(Investing.com,"DEU1Y.csv")))
Investing.csv.xts(DEU1Y, type = "y")
DEU1Y<-to.daily(DEU1Y)

HKG1Y<-data.frame(read.csv(file.path(Investing.com,"HKG1Y.csv")))
Investing.csv.xts(HKG1Y, type = "y")
HKG1Y<-to.daily(HKG1Y)

HUN1Y<-data.frame(read.csv(file.path(Investing.com,"HUN1Y.csv")))
Investing.csv.xts(HUN1Y, type = "y")
HUN1Y<-to.daily(HUN1Y)

IND1Y<-data.frame(read.csv(file.path(Investing.com,"IND1Y.csv")))
Investing.csv.xts(IND1Y, type = "y")
IND1Y<-to.daily(IND1Y)

IRL2Y<-data.frame(read.csv(file.path(Investing.com,"IRL2Y.csv")))
Investing.csv.xts(IRL2Y, type = "y")
IRL2Y<-to.daily(IRL2Y)

IRL1Y<-data.frame(read.csv(file.path(Investing.com,"IRL1Y.csv")))
Investing.csv.xts(IRL1Y, type = "y")
IRL1Y<-to.daily(IRL1Y)

ISR1Y<-data.frame(read.csv(file.path(Investing.com,"ISR1Y.csv")))
Investing.csv.xts(ISR1Y, type = "y")
ISR1Y<-to.daily(ISR1Y)

ITA1Y<-data.frame(read.csv(file.path(Investing.com,"ITA1Y.csv")))
Investing.csv.xts(ITA1Y, type = "y")
ITA1Y<-to.daily(ITA1Y)

JPN1Y<-data.frame(read.csv(file.path(Investing.com,"JPN1Y.csv")))
Investing.csv.xts(JPN1Y, type = "y")
JPN1Y<-to.daily(JPN1Y)

JOR1Y<-data.frame(read.csv(file.path(Investing.com,"JOR1Y.csv")))
Investing.csv.xts(JOR1Y, type = "y")
JOR1Y<-to.daily(JOR1Y)

KEN1Y<-data.frame(read.csv(file.path(Investing.com,"KEN1Y.csv")))
Investing.csv.xts(KEN1Y, type = "y")
KEN1Y<-to.daily(KEN1Y)

MYS1Y<-data.frame(read.csv(file.path(Investing.com,"MYS1Y.csv")))
Investing.csv.xts(MYS1Y, type = "y")
MYS1Y<-to.daily(MYS1Y)

MLT1Y<-data.frame(read.csv(file.path(Investing.com,"MLT1Y.csv")))
Investing.csv.xts(MLT1Y, type = "y")
MLT1Y<-to.daily(MLT1Y)

MUS1Y<-data.frame(read.csv(file.path(Investing.com,"MUS1Y.csv")))
Investing.csv.xts(MUS1Y, type = "y")
MUS1Y<-to.daily(MUS1Y)

MEX1Y<-data.frame(read.csv(file.path(Investing.com,"MEX1Y.csv")))
Investing.csv.xts(MEX1Y, type = "y")
MEX1Y<-to.daily(MEX1Y)

NAM1Y<-data.frame(read.csv(file.path(Investing.com,"NAM1Y.csv")))
Investing.csv.xts(NAM1Y, type = "y")
NAM1Y<-to.daily(NAM1Y)

NZL1Y<-data.frame(read.csv(file.path(Investing.com,"NZL1Y.csv")))
Investing.csv.xts(NZL1Y, type = "y")
NZL1Y<-to.daily(NZL1Y)

NGA1Y<-data.frame(read.csv(file.path(Investing.com,"NGA1Y.csv")))
Investing.csv.xts(NGA1Y, type = "y")
NGA1Y<-to.daily(NGA1Y)

NOR1Y<-data.frame(read.csv(file.path(Investing.com,"NOR1Y.csv")))
Investing.csv.xts(NOR1Y, type = "y")
NOR1Y<-to.daily(NOR1Y)

PAK1Y<-data.frame(read.csv(file.path(Investing.com,"PAK1Y.csv")))
Investing.csv.xts(PAK1Y, type = "y")
PAK1Y<-to.daily(PAK1Y)

PHL1Y<-data.frame(read.csv(file.path(Investing.com,"PHL1Y.csv")))
Investing.csv.xts(PHL1Y, type = "y")
PHL1Y<-to.daily(PHL1Y)

POL1Y<-data.frame(read.csv(file.path(Investing.com,"POL1Y.csv")))
Investing.csv.xts(POL1Y, type = "y")
POL1Y<-to.daily(POL1Y)

PRT1Y<-data.frame(read.csv(file.path(Investing.com,"PRT1Y.csv")))
Investing.csv.xts(PRT1Y, type = "y")
PRT1Y<-to.daily(PRT1Y)

ROU1Y<-data.frame(read.csv(file.path(Investing.com,"ROU1Y.csv")))
Investing.csv.xts(ROU1Y, type = "y")
ROU1Y<-to.daily(ROU1Y)

RUS1Y<-data.frame(read.csv(file.path(Investing.com,"RUS1Y.csv")))
Investing.csv.xts(RUS1Y, type = "y")
RUS1Y<-to.daily(RUS1Y)

SRB1Y<-data.frame(read.csv(file.path(Investing.com,"SRB1Y.csv")))
Investing.csv.xts(SRB1Y, type = "y")
SRB1Y<-to.daily(SRB1Y)

SGP1Y<-data.frame(read.csv(file.path(Investing.com,"SGP1Y.csv")))
Investing.csv.xts(SGP1Y, type = "y")
SGP1Y<-to.daily(SGP1Y)

SVK1Y<-data.frame(read.csv(file.path(Investing.com,"SVK1Y.csv")))
Investing.csv.xts(SVK1Y, type = "y")
SVK1Y<-to.daily(SVK1Y)

SVN1Y<-data.frame(read.csv(file.path(Investing.com,"SVN1Y.csv")))
Investing.csv.xts(SVN1Y, type = "y")
SVN1Y<-to.daily(SVN1Y)

KOR1Y<-data.frame(read.csv(file.path(Investing.com,"KOR1Y.csv")))
Investing.csv.xts(KOR1Y, type = "y")
KOR1Y<-to.daily(KOR1Y)

ESP1Y<-data.frame(read.csv(file.path(Investing.com,"ESP1Y.csv")))
Investing.csv.xts(ESP1Y, type = "y")
ESP1Y<-to.daily(ESP1Y)

LKA1Y<-data.frame(read.csv(file.path(Investing.com,"LKA1Y.csv")))
Investing.csv.xts(LKA1Y, type = "y")
LKA1Y<-to.daily(LKA1Y)

CHE1Y<-data.frame(read.csv(file.path(Investing.com,"CHE1Y.csv")))
Investing.csv.xts(CHE1Y, type = "y")
CHE1Y<-to.daily(CHE1Y)

THA1Y<-data.frame(read.csv(file.path(Investing.com,"THA1Y.csv")))
Investing.csv.xts(THA1Y, type = "y")
THA1Y<-to.daily(THA1Y)

TUR1Y<-data.frame(read.csv(file.path(Investing.com,"TUR1Y.csv")))
Investing.csv.xts(TUR1Y, type = "y")
TUR1Y<-to.daily(TUR1Y)

UGA1Y<-data.frame(read.csv(file.path(Investing.com,"UGA1Y.csv")))
Investing.csv.xts(UGA1Y, type = "y")
UGA1Y<-to.daily(UGA1Y)

UKR1Y<-data.frame(read.csv(file.path(Investing.com,"UKR1Y.csv")))
Investing.csv.xts(UKR1Y, type = "y")
UKR1Y<-to.daily(UKR1Y)

GBR1Y<-data.frame(read.csv(file.path(Investing.com,"GBR1Y.csv")))
Investing.csv.xts(GBR1Y, type = "y")
GBR1Y<-to.daily(GBR1Y)

USA1Y<-data.frame(read.csv(file.path(Investing.com,"USA1Y.csv")))
Investing.csv.xts(USA1Y, type = "y")
USA1Y<-to.daily(USA1Y)

VNM1Y<-data.frame(read.csv(file.path(Investing.com,"VNM1Y.csv")))
Investing.csv.xts(VNM1Y, type = "y")
VNM1Y<-to.daily(VNM1Y)

AUS2Y<-data.frame(read.csv(file.path(Investing.com,"AUS2Y.csv")))
Investing.csv.xts(AUS2Y, type = "y")
AUS2Y<-to.daily(AUS2Y)

AUT2Y<-data.frame(read.csv(file.path(Investing.com,"AUT2Y.csv")))
Investing.csv.xts(AUT2Y, type = "y")
AUT2Y<-to.daily(AUT2Y)

BHR2Y<-data.frame(read.csv(file.path(Investing.com,"BHR2Y.csv")))
Investing.csv.xts(BHR2Y, type = "y")
BHR2Y<-to.daily(BHR2Y)

BGD2Y<-data.frame(read.csv(file.path(Investing.com,"BGD2Y.csv")))
Investing.csv.xts(BGD2Y, type = "y")
BGD2Y<-to.daily(BGD2Y)

BEL2Y<-data.frame(read.csv(file.path(Investing.com,"BEL2Y.csv")))
Investing.csv.xts(BEL2Y, type = "y")
BEL2Y<-to.daily(BEL2Y)

BRA2Y<-data.frame(read.csv(file.path(Investing.com,"BRA2Y.csv")))
Investing.csv.xts(BRA2Y, type = "y")
BRA2Y<-to.daily(BRA2Y)

CAN2Y<-data.frame(read.csv(file.path(Investing.com,"CAN2Y.csv")))
Investing.csv.xts(CAN2Y, type = "y")
CAN2Y<-to.daily(CAN2Y)

CHL2Y<-data.frame(read.csv(file.path(Investing.com,"CHL2Y.csv")))
Investing.csv.xts(CHL2Y, type = "y")
CHL2Y<-to.daily(CHL2Y)

CHN2Y<-data.frame(read.csv(file.path(Investing.com,"CHN2Y.csv")))
Investing.csv.xts(CHN2Y, type = "y")
CHN2Y<-to.daily(CHN2Y)

CZE2Y<-data.frame(read.csv(file.path(Investing.com,"CZE2Y.csv")))
Investing.csv.xts(CZE2Y, type = "y")
CZE2Y<-to.daily(CZE2Y)

DNK2Y<-data.frame(read.csv(file.path(Investing.com,"DNK2Y.csv")))
Investing.csv.xts(DNK2Y, type = "y")
DNK2Y<-to.daily(DNK2Y)

EGY2Y<-data.frame(read.csv(file.path(Investing.com,"EGY2Y.csv")))
Investing.csv.xts(EGY2Y, type = "y")
EGY2Y<-to.daily(EGY2Y)

FIN2Y<-data.frame(read.csv(file.path(Investing.com,"FIN2Y.csv")))
Investing.csv.xts(FIN2Y, type = "y")
FIN2Y<-to.daily(FIN2Y)

FRA2Y<-data.frame(read.csv(file.path(Investing.com,"FRA2Y.csv")))
Investing.csv.xts(FRA2Y, type = "y")
FRA2Y<-to.daily(FRA2Y)

DEU2Y<-data.frame(read.csv(file.path(Investing.com,"DEU2Y.csv")))
Investing.csv.xts(DEU2Y, type = "y")
DEU2Y<-to.daily(DEU2Y)

HKG2Y<-data.frame(read.csv(file.path(Investing.com,"HKG2Y.csv")))
Investing.csv.xts(HKG2Y, type = "y")
HKG2Y<-to.daily(HKG2Y)

ISL2Y<-data.frame(read.csv(file.path(Investing.com,"ISL2Y.csv")))
Investing.csv.xts(ISL2Y, type = "y")
ISL2Y<-to.daily(ISL2Y)

IND2Y<-data.frame(read.csv(file.path(Investing.com,"IND2Y.csv")))
Investing.csv.xts(IND2Y, type = "y")
IND2Y<-to.daily(IND2Y)

IRL3Y<-data.frame(read.csv(file.path(Investing.com,"IRL3Y.csv")))
Investing.csv.xts(IRL3Y, type = "y")
IRL3Y<-to.daily(IRL3Y)

ISR2Y<-data.frame(read.csv(file.path(Investing.com,"ISR2Y.csv")))
Investing.csv.xts(ISR2Y, type = "y")
ISR2Y<-to.daily(ISR2Y)

ITA2Y<-data.frame(read.csv(file.path(Investing.com,"ITA2Y.csv")))
Investing.csv.xts(ITA2Y, type = "y")
ITA2Y<-to.daily(ITA2Y)

JPN2Y<-data.frame(read.csv(file.path(Investing.com,"JPN2Y.csv")))
Investing.csv.xts(JPN2Y, type = "y")
JPN2Y<-to.daily(JPN2Y)

JOR2Y<-data.frame(read.csv(file.path(Investing.com,"JOR2Y.csv")))
Investing.csv.xts(JOR2Y, type = "y")
JOR2Y<-to.daily(JOR2Y)

KEN2Y<-data.frame(read.csv(file.path(Investing.com,"KEN2Y.csv")))
Investing.csv.xts(KEN2Y, type = "y")
KEN2Y<-to.daily(KEN2Y)

LVA2Y<-data.frame(read.csv(file.path(Investing.com,"LVA2Y.csv")))
Investing.csv.xts(LVA2Y, type = "y")
LVA2Y<-to.daily(LVA2Y)

MUS2Y<-data.frame(read.csv(file.path(Investing.com,"MUS2Y.csv")))
Investing.csv.xts(MUS2Y, type = "y")
MUS2Y<-to.daily(MUS2Y)

MAR2Y<-data.frame(read.csv(file.path(Investing.com,"MAR2Y.csv")))
Investing.csv.xts(MAR2Y, type = "y")
MAR2Y<-to.daily(MAR2Y)

NLD2Y<-data.frame(read.csv(file.path(Investing.com,"NLD2Y.csv")))
Investing.csv.xts(NLD2Y, type = "y")
NLD2Y<-to.daily(NLD2Y)

NZL2Y<-data.frame(read.csv(file.path(Investing.com,"NZL2Y.csv")))
Investing.csv.xts(NZL2Y, type = "y")
NZL2Y<-to.daily(NZL2Y)

NGA2Y<-data.frame(read.csv(file.path(Investing.com,"NGA2Y.csv")))
Investing.csv.xts(NGA2Y, type = "y")
NGA2Y<-to.daily(NGA2Y)

PHL2Y<-data.frame(read.csv(file.path(Investing.com,"PHL2Y.csv")))
Investing.csv.xts(PHL2Y, type = "y")
PHL2Y<-to.daily(PHL2Y)

POL2Y<-data.frame(read.csv(file.path(Investing.com,"POL2Y.csv")))
Investing.csv.xts(POL2Y, type = "y")
POL2Y<-to.daily(POL2Y)

PRT2Y<-data.frame(read.csv(file.path(Investing.com,"PRT2Y.csv")))
Investing.csv.xts(PRT2Y, type = "y")
PRT2Y<-to.daily(PRT2Y)

ROU2Y<-data.frame(read.csv(file.path(Investing.com,"ROU2Y.csv")))
Investing.csv.xts(ROU2Y, type = "y")
ROU2Y<-to.daily(ROU2Y)

RUS2Y<-data.frame(read.csv(file.path(Investing.com,"RUS2Y.csv")))
Investing.csv.xts(RUS2Y, type = "y")
RUS2Y<-to.daily(RUS2Y)

SRB2Y<-data.frame(read.csv(file.path(Investing.com,"SRB2Y.csv")))
Investing.csv.xts(SRB2Y, type = "y")
SRB2Y<-to.daily(SRB2Y)

SGP2Y<-data.frame(read.csv(file.path(Investing.com,"SGP2Y.csv")))
Investing.csv.xts(SGP2Y, type = "y")
SGP2Y<-to.daily(SGP2Y)

SVK2Y<-data.frame(read.csv(file.path(Investing.com,"SVK2Y.csv")))
Investing.csv.xts(SVK2Y, type = "y")
SVK2Y<-to.daily(SVK2Y)

SVN2Y<-data.frame(read.csv(file.path(Investing.com,"SVN2Y.csv")))
Investing.csv.xts(SVN2Y, type = "y")
SVN2Y<-to.daily(SVN2Y)

ZAF2Y<-data.frame(read.csv(file.path(Investing.com,"ZAF2Y.csv")))
Investing.csv.xts(ZAF2Y, type = "y")
ZAF2Y<-to.daily(ZAF2Y)

KOR2Y<-data.frame(read.csv(file.path(Investing.com,"KOR2Y.csv")))
Investing.csv.xts(KOR2Y, type = "y")
KOR2Y<-to.daily(KOR2Y)

ESP2Y<-data.frame(read.csv(file.path(Investing.com,"ESP2Y.csv")))
Investing.csv.xts(ESP2Y, type = "y")
ESP2Y<-to.daily(ESP2Y)

LKA2Y<-data.frame(read.csv(file.path(Investing.com,"LKA2Y.csv")))
Investing.csv.xts(LKA2Y, type = "y")
LKA2Y<-to.daily(LKA2Y)

SWE2Y<-data.frame(read.csv(file.path(Investing.com,"SWE2Y.csv")))
Investing.csv.xts(SWE2Y, type = "y")
SWE2Y<-to.daily(SWE2Y)

CHE2Y<-data.frame(read.csv(file.path(Investing.com,"CHE2Y.csv")))
Investing.csv.xts(CHE2Y, type = "y")
CHE2Y<-to.daily(CHE2Y)

TWN2Y<-data.frame(read.csv(file.path(Investing.com,"TWN2Y.csv")))
Investing.csv.xts(TWN2Y, type = "y")
TWN2Y<-to.daily(TWN2Y)

THA2Y<-data.frame(read.csv(file.path(Investing.com,"THA2Y.csv")))
Investing.csv.xts(THA2Y, type = "y")
THA2Y<-to.daily(THA2Y)

TUR2Y<-data.frame(read.csv(file.path(Investing.com,"TUR2Y.csv")))
Investing.csv.xts(TUR2Y, type = "y")
TUR2Y<-to.daily(TUR2Y)

UGA2Y<-data.frame(read.csv(file.path(Investing.com,"UGA2Y.csv")))
Investing.csv.xts(UGA2Y, type = "y")
UGA2Y<-to.daily(UGA2Y)

UKR2Y<-data.frame(read.csv(file.path(Investing.com,"UKR2Y.csv")))
Investing.csv.xts(UKR2Y, type = "y")
UKR2Y<-to.daily(UKR2Y)

GBR2Y<-data.frame(read.csv(file.path(Investing.com,"GBR2Y.csv")))
Investing.csv.xts(GBR2Y, type = "y")
GBR2Y<-to.daily(GBR2Y)

USA2Y<-data.frame(read.csv(file.path(Investing.com,"USA2Y.csv")))
Investing.csv.xts(USA2Y, type = "y")
USA2Y<-to.daily(USA2Y)

VEN2Y<-data.frame(read.csv(file.path(Investing.com,"VEN2Y.csv")))
Investing.csv.xts(VEN2Y, type = "y")
VEN2Y<-to.daily(VEN2Y)

VNM2Y<-data.frame(read.csv(file.path(Investing.com,"VNM2Y.csv")))
Investing.csv.xts(VNM2Y, type = "y")
VNM2Y<-to.daily(VNM2Y)

AUS3Y<-data.frame(read.csv(file.path(Investing.com,"AUS3Y.csv")))
Investing.csv.xts(AUS3Y, type = "y")
AUS3Y<-to.daily(AUS3Y)

AUT3Y<-data.frame(read.csv(file.path(Investing.com,"AUT3Y.csv")))
Investing.csv.xts(AUT3Y, type = "y")
AUT3Y<-to.daily(AUT3Y)

BEL3Y<-data.frame(read.csv(file.path(Investing.com,"BEL3Y.csv")))
Investing.csv.xts(BEL3Y, type = "y")
BEL3Y<-to.daily(BEL3Y)

BWA3Y<-data.frame(read.csv(file.path(Investing.com,"BWA3Y.csv")))
Investing.csv.xts(BWA3Y, type = "y")
BWA3Y<-to.daily(BWA3Y)

BRA3Y<-data.frame(read.csv(file.path(Investing.com,"BRA3Y.csv")))
Investing.csv.xts(BRA3Y, type = "y")
BRA3Y<-to.daily(BRA3Y)

BGR3Y<-data.frame(read.csv(file.path(Investing.com,"BGR3Y.csv")))
Investing.csv.xts(BGR3Y, type = "y")
BGR3Y<-to.daily(BGR3Y)

CAN3Y<-data.frame(read.csv(file.path(Investing.com,"CAN3Y.csv")))
Investing.csv.xts(CAN3Y, type = "y")
CAN3Y<-to.daily(CAN3Y)

CHL3Y<-data.frame(read.csv(file.path(Investing.com,"CHL3Y.csv")))
Investing.csv.xts(CHL3Y, type = "y")
CHL3Y<-to.daily(CHL3Y)

CHN3Y<-data.frame(read.csv(file.path(Investing.com,"CHN3Y.csv")))
Investing.csv.xts(CHN3Y, type = "y")
CHN3Y<-to.daily(CHN3Y)

HRV3Y<-data.frame(read.csv(file.path(Investing.com,"HRV3Y.csv")))
Investing.csv.xts(HRV3Y, type = "y")
HRV3Y<-to.daily(HRV3Y)

CZE3Y<-data.frame(read.csv(file.path(Investing.com,"CZE3Y.csv")))
Investing.csv.xts(CZE3Y, type = "y")
CZE3Y<-to.daily(CZE3Y)

DNK3Y<-data.frame(read.csv(file.path(Investing.com,"DNK3Y.csv")))
Investing.csv.xts(DNK3Y, type = "y")
DNK3Y<-to.daily(DNK3Y)

EGY3Y<-data.frame(read.csv(file.path(Investing.com,"EGY3Y.csv")))
Investing.csv.xts(EGY3Y, type = "y")
EGY3Y<-to.daily(EGY3Y)

FIN3Y<-data.frame(read.csv(file.path(Investing.com,"FIN3Y.csv")))
Investing.csv.xts(FIN3Y, type = "y")
FIN3Y<-to.daily(FIN3Y)

FRA3Y<-data.frame(read.csv(file.path(Investing.com,"FRA3Y.csv")))
Investing.csv.xts(FRA3Y, type = "y")
FRA3Y<-to.daily(FRA3Y)

DEU3Y<-data.frame(read.csv(file.path(Investing.com,"DEU3Y.csv")))
Investing.csv.xts(DEU3Y, type = "y")
DEU3Y<-to.daily(DEU3Y)

HKG3Y<-data.frame(read.csv(file.path(Investing.com,"HKG3Y.csv")))
Investing.csv.xts(HKG3Y, type = "y")
HKG3Y<-to.daily(HKG3Y)

HUN3Y<-data.frame(read.csv(file.path(Investing.com,"HUN3Y.csv")))
Investing.csv.xts(HUN3Y, type = "y")
HUN3Y<-to.daily(HUN3Y)

IND3Y<-data.frame(read.csv(file.path(Investing.com,"IND3Y.csv")))
Investing.csv.xts(IND3Y, type = "y")
IND3Y<-to.daily(IND3Y)

IDN3Y<-data.frame(read.csv(file.path(Investing.com,"IDN3Y.csv")))
Investing.csv.xts(IDN3Y, type = "y")
IDN3Y<-to.daily(IDN3Y)

ISR3Y<-data.frame(read.csv(file.path(Investing.com,"ISR3Y.csv")))
Investing.csv.xts(ISR3Y, type = "y")
ISR3Y<-to.daily(ISR3Y)

ITA3Y<-data.frame(read.csv(file.path(Investing.com,"ITA3Y.csv")))
Investing.csv.xts(ITA3Y, type = "y")
ITA3Y<-to.daily(ITA3Y)

JPN3Y<-data.frame(read.csv(file.path(Investing.com,"JPN3Y.csv")))
Investing.csv.xts(JPN3Y, type = "y")
JPN3Y<-to.daily(JPN3Y)

JOR3Y<-data.frame(read.csv(file.path(Investing.com,"JOR3Y.csv")))
Investing.csv.xts(JOR3Y, type = "y")
JOR3Y<-to.daily(JOR3Y)

KEN3Y<-data.frame(read.csv(file.path(Investing.com,"KEN3Y.csv")))
Investing.csv.xts(KEN3Y, type = "y")
KEN3Y<-to.daily(KEN3Y)

LVA3Y<-data.frame(read.csv(file.path(Investing.com,"LVA3Y.csv")))
Investing.csv.xts(LVA3Y, type = "y")
LVA3Y<-to.daily(LVA3Y)

LTU3Y<-data.frame(read.csv(file.path(Investing.com,"LTU3Y.csv")))
Investing.csv.xts(LTU3Y, type = "y")
LTU3Y<-to.daily(LTU3Y)

MYS3Y<-data.frame(read.csv(file.path(Investing.com,"MYS3Y.csv")))
Investing.csv.xts(MYS3Y, type = "y")
MYS3Y<-to.daily(MYS3Y)

MLT3Y<-data.frame(read.csv(file.path(Investing.com,"MLT3Y.csv")))
Investing.csv.xts(MLT3Y, type = "y")
MLT3Y<-to.daily(MLT3Y)

MUS3Y<-data.frame(read.csv(file.path(Investing.com,"MUS3Y.csv")))
Investing.csv.xts(MUS3Y, type = "y")
MUS3Y<-to.daily(MUS3Y)

MEX3Y<-data.frame(read.csv(file.path(Investing.com,"MEX3Y.csv")))
Investing.csv.xts(MEX3Y, type = "y")
MEX3Y<-to.daily(MEX3Y)

NAM3Y<-data.frame(read.csv(file.path(Investing.com,"NAM3Y.csv")))
Investing.csv.xts(NAM3Y, type = "y")
NAM3Y<-to.daily(NAM3Y)

NLD3Y<-data.frame(read.csv(file.path(Investing.com,"NLD3Y.csv")))
Investing.csv.xts(NLD3Y, type = "y")
NLD3Y<-to.daily(NLD3Y)

NOR3Y<-data.frame(read.csv(file.path(Investing.com,"NOR3Y.csv")))
Investing.csv.xts(NOR3Y, type = "y")
NOR3Y<-to.daily(NOR3Y)

PAK3Y<-data.frame(read.csv(file.path(Investing.com,"PAK3Y.csv")))
Investing.csv.xts(PAK3Y, type = "y")
PAK3Y<-to.daily(PAK3Y)

PHL3Y<-data.frame(read.csv(file.path(Investing.com,"PHL3Y.csv")))
Investing.csv.xts(PHL3Y, type = "y")
PHL3Y<-to.daily(PHL3Y)

POL3Y<-data.frame(read.csv(file.path(Investing.com,"POL3Y.csv")))
Investing.csv.xts(POL3Y, type = "y")
POL3Y<-to.daily(POL3Y)

PRT3Y<-data.frame(read.csv(file.path(Investing.com,"PRT3Y.csv")))
Investing.csv.xts(PRT3Y, type = "y")
PRT3Y<-to.daily(PRT3Y)

ROU3Y<-data.frame(read.csv(file.path(Investing.com,"ROU3Y.csv")))
Investing.csv.xts(ROU3Y, type = "y")
ROU3Y<-to.daily(ROU3Y)

RUS3Y<-data.frame(read.csv(file.path(Investing.com,"RUS3Y.csv")))
Investing.csv.xts(RUS3Y, type = "y")
RUS3Y<-to.daily(RUS3Y)

SVK3Y<-data.frame(read.csv(file.path(Investing.com,"SVK3Y.csv")))
Investing.csv.xts(SVK3Y, type = "y")
SVK3Y<-to.daily(SVK3Y)

ZAF3Y<-data.frame(read.csv(file.path(Investing.com,"ZAF3Y.csv")))
Investing.csv.xts(ZAF3Y, type = "y")
ZAF3Y<-to.daily(ZAF3Y)

KOR3Y<-data.frame(read.csv(file.path(Investing.com,"KOR3Y.csv")))
Investing.csv.xts(KOR3Y, type = "y")
KOR3Y<-to.daily(KOR3Y)

ESP3Y<-data.frame(read.csv(file.path(Investing.com,"ESP3Y.csv")))
Investing.csv.xts(ESP3Y, type = "y")
ESP3Y<-to.daily(ESP3Y)

LKA3Y<-data.frame(read.csv(file.path(Investing.com,"LKA3Y.csv")))
Investing.csv.xts(LKA3Y, type = "y")
LKA3Y<-to.daily(LKA3Y)

CHE3Y<-data.frame(read.csv(file.path(Investing.com,"CHE3Y.csv")))
Investing.csv.xts(CHE3Y, type = "y")
CHE3Y<-to.daily(CHE3Y)

THA3Y<-data.frame(read.csv(file.path(Investing.com,"THA3Y.csv")))
Investing.csv.xts(THA3Y, type = "y")
THA3Y<-to.daily(THA3Y)

TUR3Y<-data.frame(read.csv(file.path(Investing.com,"TUR3Y.csv")))
Investing.csv.xts(TUR3Y, type = "y")
TUR3Y<-to.daily(TUR3Y)

UGA3Y<-data.frame(read.csv(file.path(Investing.com,"UGA3Y.csv")))
Investing.csv.xts(UGA3Y, type = "y")
UGA3Y<-to.daily(UGA3Y)

UKR3Y<-data.frame(read.csv(file.path(Investing.com,"UKR3Y.csv")))
Investing.csv.xts(UKR3Y, type = "y")
UKR3Y<-to.daily(UKR3Y)

GBR3Y<-data.frame(read.csv(file.path(Investing.com,"GBR3Y.csv")))
Investing.csv.xts(GBR3Y, type = "y")
GBR3Y<-to.daily(GBR3Y)

USA3Y<-data.frame(read.csv(file.path(Investing.com,"USA3Y.csv")))
Investing.csv.xts(USA3Y, type = "y")
USA3Y<-to.daily(USA3Y)

VNM3Y<-data.frame(read.csv(file.path(Investing.com,"VNM3Y.csv")))
Investing.csv.xts(VNM3Y, type = "y")
VNM3Y<-to.daily(VNM3Y)

ARG4Y<-data.frame(read.csv(file.path(Investing.com,"ARG4Y.csv")))
Investing.csv.xts(ARG4Y, type = "y")
ARG4Y<-to.daily(ARG4Y)

AUS4Y<-data.frame(read.csv(file.path(Investing.com,"AUS4Y.csv")))
Investing.csv.xts(AUS4Y, type = "y")
AUS4Y<-to.daily(AUS4Y)

AUT4Y<-data.frame(read.csv(file.path(Investing.com,"AUT4Y.csv")))
Investing.csv.xts(AUT4Y, type = "y")
AUT4Y<-to.daily(AUT4Y)

BEL4Y<-data.frame(read.csv(file.path(Investing.com,"BEL4Y.csv")))
Investing.csv.xts(BEL4Y, type = "y")
BEL4Y<-to.daily(BEL4Y)

CAN4Y<-data.frame(read.csv(file.path(Investing.com,"CAN4Y.csv")))
Investing.csv.xts(CAN4Y, type = "y")
CAN4Y<-to.daily(CAN4Y)

CHL4Y<-data.frame(read.csv(file.path(Investing.com,"CHL4Y.csv")))
Investing.csv.xts(CHL4Y, type = "y")
CHL4Y<-to.daily(CHL4Y)

COL4Y<-data.frame(read.csv(file.path(Investing.com,"COL4Y.csv")))
Investing.csv.xts(COL4Y, type = "y")
COL4Y<-to.daily(COL4Y)

CZE4Y<-data.frame(read.csv(file.path(Investing.com,"CZE4Y.csv")))
Investing.csv.xts(CZE4Y, type = "y")
CZE4Y<-to.daily(CZE4Y)

FIN4Y<-data.frame(read.csv(file.path(Investing.com,"FIN4Y.csv")))
Investing.csv.xts(FIN4Y, type = "y")
FIN4Y<-to.daily(FIN4Y)

FRA4Y<-data.frame(read.csv(file.path(Investing.com,"FRA4Y.csv")))
Investing.csv.xts(FRA4Y, type = "y")
FRA4Y<-to.daily(FRA4Y)

DEU4Y<-data.frame(read.csv(file.path(Investing.com,"DEU4Y.csv")))
Investing.csv.xts(DEU4Y, type = "y")
DEU4Y<-to.daily(DEU4Y)

IND4Y<-data.frame(read.csv(file.path(Investing.com,"IND4Y.csv")))
Investing.csv.xts(IND4Y, type = "y")
IND4Y<-to.daily(IND4Y)

IRL5Y<-data.frame(read.csv(file.path(Investing.com,"IRL5Y.csv")))
Investing.csv.xts(IRL5Y, type = "y")
IRL5Y<-to.daily(IRL5Y)

IRL4Y<-data.frame(read.csv(file.path(Investing.com,"IRL4Y.csv")))
Investing.csv.xts(IRL4Y, type = "y")
IRL4Y<-to.daily(IRL4Y)

ITA4Y<-data.frame(read.csv(file.path(Investing.com,"ITA4Y.csv")))
Investing.csv.xts(ITA4Y, type = "y")
ITA4Y<-to.daily(ITA4Y)

JPN4Y<-data.frame(read.csv(file.path(Investing.com,"JPN4Y.csv")))
Investing.csv.xts(JPN4Y, type = "y")
JPN4Y<-to.daily(JPN4Y)

KEN4Y<-data.frame(read.csv(file.path(Investing.com,"KEN4Y.csv")))
Investing.csv.xts(KEN4Y, type = "y")
KEN4Y<-to.daily(KEN4Y)

MUS4Y<-data.frame(read.csv(file.path(Investing.com,"MUS4Y.csv")))
Investing.csv.xts(MUS4Y, type = "y")
MUS4Y<-to.daily(MUS4Y)

NLD4Y<-data.frame(read.csv(file.path(Investing.com,"NLD4Y.csv")))
Investing.csv.xts(NLD4Y, type = "y")
NLD4Y<-to.daily(NLD4Y)

NGA4Y<-data.frame(read.csv(file.path(Investing.com,"NGA4Y.csv")))
Investing.csv.xts(NGA4Y, type = "y")
NGA4Y<-to.daily(NGA4Y)

PHL4Y<-data.frame(read.csv(file.path(Investing.com,"PHL4Y.csv")))
Investing.csv.xts(PHL4Y, type = "y")
PHL4Y<-to.daily(PHL4Y)

POL4Y<-data.frame(read.csv(file.path(Investing.com,"POL4Y.csv")))
Investing.csv.xts(POL4Y, type = "y")
POL4Y<-to.daily(POL4Y)

PRT4Y<-data.frame(read.csv(file.path(Investing.com,"PRT4Y.csv")))
Investing.csv.xts(PRT4Y, type = "y")
PRT4Y<-to.daily(PRT4Y)

ROU4Y<-data.frame(read.csv(file.path(Investing.com,"ROU4Y.csv")))
Investing.csv.xts(ROU4Y, type = "y")
ROU4Y<-to.daily(ROU4Y)

SRB4Y<-data.frame(read.csv(file.path(Investing.com,"SRB4Y.csv")))
Investing.csv.xts(SRB4Y, type = "y")
SRB4Y<-to.daily(SRB4Y)

KOR4Y<-data.frame(read.csv(file.path(Investing.com,"KOR4Y.csv")))
Investing.csv.xts(KOR4Y, type = "y")
KOR4Y<-to.daily(KOR4Y)

ESP4Y<-data.frame(read.csv(file.path(Investing.com,"ESP4Y.csv")))
Investing.csv.xts(ESP4Y, type = "y")
ESP4Y<-to.daily(ESP4Y)

LKA4Y<-data.frame(read.csv(file.path(Investing.com,"LKA4Y.csv")))
Investing.csv.xts(LKA4Y, type = "y")
LKA4Y<-to.daily(LKA4Y)

CHE4Y<-data.frame(read.csv(file.path(Investing.com,"CHE4Y.csv")))
Investing.csv.xts(CHE4Y, type = "y")
CHE4Y<-to.daily(CHE4Y)

UGA4Y<-data.frame(read.csv(file.path(Investing.com,"UGA4Y.csv")))
Investing.csv.xts(UGA4Y, type = "y")
UGA4Y<-to.daily(UGA4Y)

GBR4Y<-data.frame(read.csv(file.path(Investing.com,"GBR4Y.csv")))
Investing.csv.xts(GBR4Y, type = "y")
GBR4Y<-to.daily(GBR4Y)

AUS5Y<-data.frame(read.csv(file.path(Investing.com,"AUS5Y.csv")))
Investing.csv.xts(AUS5Y, type = "y")
AUS5Y<-to.daily(AUS5Y)

AUT5Y<-data.frame(read.csv(file.path(Investing.com,"AUT5Y.csv")))
Investing.csv.xts(AUT5Y, type = "y")
AUT5Y<-to.daily(AUT5Y)

BHR5Y<-data.frame(read.csv(file.path(Investing.com,"BHR5Y.csv")))
Investing.csv.xts(BHR5Y, type = "y")
BHR5Y<-to.daily(BHR5Y)

BGD5Y<-data.frame(read.csv(file.path(Investing.com,"BGD5Y.csv")))
Investing.csv.xts(BGD5Y, type = "y")
BGD5Y<-to.daily(BGD5Y)

BEL5Y<-data.frame(read.csv(file.path(Investing.com,"BEL5Y.csv")))
Investing.csv.xts(BEL5Y, type = "y")
BEL5Y<-to.daily(BEL5Y)

BWA5Y<-data.frame(read.csv(file.path(Investing.com,"BWA5Y.csv")))
Investing.csv.xts(BWA5Y, type = "y")
BWA5Y<-to.daily(BWA5Y)

BRA5Y<-data.frame(read.csv(file.path(Investing.com,"BRA5Y.csv")))
Investing.csv.xts(BRA5Y, type = "y")
BRA5Y<-to.daily(BRA5Y)

BGR5Y<-data.frame(read.csv(file.path(Investing.com,"BGR5Y.csv")))
Investing.csv.xts(BGR5Y, type = "y")
BGR5Y<-to.daily(BGR5Y)

CAN5Y<-data.frame(read.csv(file.path(Investing.com,"CAN5Y.csv")))
Investing.csv.xts(CAN5Y, type = "y")
CAN5Y<-to.daily(CAN5Y)

CHL5Y<-data.frame(read.csv(file.path(Investing.com,"CHL5Y.csv")))
Investing.csv.xts(CHL5Y, type = "y")
CHL5Y<-to.daily(CHL5Y)

CHN5Y<-data.frame(read.csv(file.path(Investing.com,"CHN5Y.csv")))
Investing.csv.xts(CHN5Y, type = "y")
CHN5Y<-to.daily(CHN5Y)

COL5Y<-data.frame(read.csv(file.path(Investing.com,"COL5Y.csv")))
Investing.csv.xts(COL5Y, type = "y")
COL5Y<-to.daily(COL5Y)

HRV5Y<-data.frame(read.csv(file.path(Investing.com,"HRV5Y.csv")))
Investing.csv.xts(HRV5Y, type = "y")
HRV5Y<-to.daily(HRV5Y)

CZE5Y<-data.frame(read.csv(file.path(Investing.com,"CZE5Y.csv")))
Investing.csv.xts(CZE5Y, type = "y")
CZE5Y<-to.daily(CZE5Y)

DNK5Y<-data.frame(read.csv(file.path(Investing.com,"DNK5Y.csv")))
Investing.csv.xts(DNK5Y, type = "y")
DNK5Y<-to.daily(DNK5Y)

EGY5Y<-data.frame(read.csv(file.path(Investing.com,"EGY5Y.csv")))
Investing.csv.xts(EGY5Y, type = "y")
EGY5Y<-to.daily(EGY5Y)

FIN5Y<-data.frame(read.csv(file.path(Investing.com,"FIN5Y.csv")))
Investing.csv.xts(FIN5Y, type = "y")
FIN5Y<-to.daily(FIN5Y)

FRA5Y<-data.frame(read.csv(file.path(Investing.com,"FRA5Y.csv")))
Investing.csv.xts(FRA5Y, type = "y")
FRA5Y<-to.daily(FRA5Y)

DEU5Y<-data.frame(read.csv(file.path(Investing.com,"DEU5Y.csv")))
Investing.csv.xts(DEU5Y, type = "y")
DEU5Y<-to.daily(DEU5Y)

GRC5Y<-data.frame(read.csv(file.path(Investing.com,"GRC5Y.csv")))
Investing.csv.xts(GRC5Y, type = "y")
GRC5Y<-to.daily(GRC5Y)

HKG5Y<-data.frame(read.csv(file.path(Investing.com,"HKG5Y.csv")))
Investing.csv.xts(HKG5Y, type = "y")
HKG5Y<-to.daily(HKG5Y)

HUN5Y<-data.frame(read.csv(file.path(Investing.com,"HUN5Y.csv")))
Investing.csv.xts(HUN5Y, type = "y")
HUN5Y<-to.daily(HUN5Y)

ISL5Y<-data.frame(read.csv(file.path(Investing.com,"ISL5Y.csv")))
Investing.csv.xts(ISL5Y, type = "y")
ISL5Y<-to.daily(ISL5Y)

IND5Y<-data.frame(read.csv(file.path(Investing.com,"IND5Y.csv")))
Investing.csv.xts(IND5Y, type = "y")
IND5Y<-to.daily(IND5Y)

IDN5Y<-data.frame(read.csv(file.path(Investing.com,"IDN5Y.csv")))
Investing.csv.xts(IDN5Y, type = "y")
IDN5Y<-to.daily(IDN5Y)

ISR5Y<-data.frame(read.csv(file.path(Investing.com,"ISR5Y.csv")))
Investing.csv.xts(ISR5Y, type = "y")
ISR5Y<-to.daily(ISR5Y)

ITA5Y<-data.frame(read.csv(file.path(Investing.com,"ITA5Y.csv")))
Investing.csv.xts(ITA5Y, type = "y")
ITA5Y<-to.daily(ITA5Y)

JPN5Y<-data.frame(read.csv(file.path(Investing.com,"JPN5Y.csv")))
Investing.csv.xts(JPN5Y, type = "y")
JPN5Y<-to.daily(JPN5Y)

JOR5Y<-data.frame(read.csv(file.path(Investing.com,"JOR5Y.csv")))
Investing.csv.xts(JOR5Y, type = "y")
JOR5Y<-to.daily(JOR5Y)

KEN5Y<-data.frame(read.csv(file.path(Investing.com,"KEN5Y.csv")))
Investing.csv.xts(KEN5Y, type = "y")
KEN5Y<-to.daily(KEN5Y)

LVA5Y<-data.frame(read.csv(file.path(Investing.com,"LVA5Y.csv")))
Investing.csv.xts(LVA5Y, type = "y")
LVA5Y<-to.daily(LVA5Y)

LTU5Y<-data.frame(read.csv(file.path(Investing.com,"LTU5Y.csv")))
Investing.csv.xts(LTU5Y, type = "y")
LTU5Y<-to.daily(LTU5Y)

MYS5Y<-data.frame(read.csv(file.path(Investing.com,"MYS5Y.csv")))
Investing.csv.xts(MYS5Y, type = "y")
MYS5Y<-to.daily(MYS5Y)

MLT5Y<-data.frame(read.csv(file.path(Investing.com,"MLT5Y.csv")))
Investing.csv.xts(MLT5Y, type = "y")
MLT5Y<-to.daily(MLT5Y)

MUS5Y<-data.frame(read.csv(file.path(Investing.com,"MUS5Y.csv")))
Investing.csv.xts(MUS5Y, type = "y")
MUS5Y<-to.daily(MUS5Y)

MEX5Y<-data.frame(read.csv(file.path(Investing.com,"MEX5Y.csv")))
Investing.csv.xts(MEX5Y, type = "y")
MEX5Y<-to.daily(MEX5Y)

MAR5Y<-data.frame(read.csv(file.path(Investing.com,"MAR5Y.csv")))
Investing.csv.xts(MAR5Y, type = "y")
MAR5Y<-to.daily(MAR5Y)

NLD5Y<-data.frame(read.csv(file.path(Investing.com,"NLD5Y.csv")))
Investing.csv.xts(NLD5Y, type = "y")
NLD5Y<-to.daily(NLD5Y)

NZL5Y<-data.frame(read.csv(file.path(Investing.com,"NZL5Y.csv")))
Investing.csv.xts(NZL5Y, type = "y")
NZL5Y<-to.daily(NZL5Y)

NGA5Y<-data.frame(read.csv(file.path(Investing.com,"NGA5Y.csv")))
Investing.csv.xts(NGA5Y, type = "y")
NGA5Y<-to.daily(NGA5Y)

NOR5Y<-data.frame(read.csv(file.path(Investing.com,"NOR5Y.csv")))
Investing.csv.xts(NOR5Y, type = "y")
NOR5Y<-to.daily(NOR5Y)

PAK5Y<-data.frame(read.csv(file.path(Investing.com,"PAK5Y.csv")))
Investing.csv.xts(PAK5Y, type = "y")
PAK5Y<-to.daily(PAK5Y)

PER5Y<-data.frame(read.csv(file.path(Investing.com,"PER5Y.csv")))
Investing.csv.xts(PER5Y, type = "y")
PER5Y<-to.daily(PER5Y)

PHL5Y<-data.frame(read.csv(file.path(Investing.com,"PHL5Y.csv")))
Investing.csv.xts(PHL5Y, type = "y")
PHL5Y<-to.daily(PHL5Y)

POL5Y<-data.frame(read.csv(file.path(Investing.com,"POL5Y.csv")))
Investing.csv.xts(POL5Y, type = "y")
POL5Y<-to.daily(POL5Y)

PRT5Y<-data.frame(read.csv(file.path(Investing.com,"PRT5Y.csv")))
Investing.csv.xts(PRT5Y, type = "y")
PRT5Y<-to.daily(PRT5Y)

QAT5Y<-data.frame(read.csv(file.path(Investing.com,"QAT5Y.csv")))
Investing.csv.xts(QAT5Y, type = "y")
QAT5Y<-to.daily(QAT5Y)

ROU5Y<-data.frame(read.csv(file.path(Investing.com,"ROU5Y.csv")))
Investing.csv.xts(ROU5Y, type = "y")
ROU5Y<-to.daily(ROU5Y)

RUS5Y<-data.frame(read.csv(file.path(Investing.com,"RUS5Y.csv")))
Investing.csv.xts(RUS5Y, type = "y")
RUS5Y<-to.daily(RUS5Y)

SRB5Y<-data.frame(read.csv(file.path(Investing.com,"SRB5Y.csv")))
Investing.csv.xts(SRB5Y, type = "y")
SRB5Y<-to.daily(SRB5Y)

SGP5Y<-data.frame(read.csv(file.path(Investing.com,"SGP5Y.csv")))
Investing.csv.xts(SGP5Y, type = "y")
SGP5Y<-to.daily(SGP5Y)

SVK5Y<-data.frame(read.csv(file.path(Investing.com,"SVK5Y.csv")))
Investing.csv.xts(SVK5Y, type = "y")
SVK5Y<-to.daily(SVK5Y)

SVN5Y<-data.frame(read.csv(file.path(Investing.com,"SVN5Y.csv")))
Investing.csv.xts(SVN5Y, type = "y")
SVN5Y<-to.daily(SVN5Y)

ZAF5Y<-data.frame(read.csv(file.path(Investing.com,"ZAF5Y.csv")))
Investing.csv.xts(ZAF5Y, type = "y")
ZAF5Y<-to.daily(ZAF5Y)

KOR5Y<-data.frame(read.csv(file.path(Investing.com,"KOR5Y.csv")))
Investing.csv.xts(KOR5Y, type = "y")
KOR5Y<-to.daily(KOR5Y)

ESP5Y<-data.frame(read.csv(file.path(Investing.com,"ESP5Y.csv")))
Investing.csv.xts(ESP5Y, type = "y")
ESP5Y<-to.daily(ESP5Y)

LKA5Y<-data.frame(read.csv(file.path(Investing.com,"LKA5Y.csv")))
Investing.csv.xts(LKA5Y, type = "y")
LKA5Y<-to.daily(LKA5Y)

SWE5Y<-data.frame(read.csv(file.path(Investing.com,"SWE5Y.csv")))
Investing.csv.xts(SWE5Y, type = "y")
SWE5Y<-to.daily(SWE5Y)

CHE5Y<-data.frame(read.csv(file.path(Investing.com,"CHE5Y.csv")))
Investing.csv.xts(CHE5Y, type = "y")
CHE5Y<-to.daily(CHE5Y)

TWN5Y<-data.frame(read.csv(file.path(Investing.com,"TWN5Y.csv")))
Investing.csv.xts(TWN5Y, type = "y")
TWN5Y<-to.daily(TWN5Y)

THA5Y<-data.frame(read.csv(file.path(Investing.com,"THA5Y.csv")))
Investing.csv.xts(THA5Y, type = "y")
THA5Y<-to.daily(THA5Y)

TUR5Y<-data.frame(read.csv(file.path(Investing.com,"TUR5Y.csv")))
Investing.csv.xts(TUR5Y, type = "y")
TUR5Y<-to.daily(TUR5Y)

UGA5Y<-data.frame(read.csv(file.path(Investing.com,"UGA5Y.csv")))
Investing.csv.xts(UGA5Y, type = "y")
UGA5Y<-to.daily(UGA5Y)

GBR5Y<-data.frame(read.csv(file.path(Investing.com,"GBR5Y.csv")))
Investing.csv.xts(GBR5Y, type = "y")
GBR5Y<-to.daily(GBR5Y)

USA5Y<-data.frame(read.csv(file.path(Investing.com,"USA5Y.csv")))
Investing.csv.xts(USA5Y, type = "y")
USA5Y<-to.daily(USA5Y)

VEN5Y<-data.frame(read.csv(file.path(Investing.com,"VEN5Y.csv")))
Investing.csv.xts(VEN5Y, type = "y")
VEN5Y<-to.daily(VEN5Y)

VNM5Y<-data.frame(read.csv(file.path(Investing.com,"VNM5Y.csv")))
Investing.csv.xts(VNM5Y, type = "y")
VNM5Y<-to.daily(VNM5Y)

ARG6Y<-data.frame(read.csv(file.path(Investing.com,"ARG6Y.csv")))
Investing.csv.xts(ARG6Y, type = "y")
ARG6Y<-to.daily(ARG6Y)

AUS6Y<-data.frame(read.csv(file.path(Investing.com,"AUS6Y.csv")))
Investing.csv.xts(AUS6Y, type = "y")
AUS6Y<-to.daily(AUS6Y)

AUT6Y<-data.frame(read.csv(file.path(Investing.com,"AUT6Y.csv")))
Investing.csv.xts(AUT6Y, type = "y")
AUT6Y<-to.daily(AUT6Y)

BEL6Y<-data.frame(read.csv(file.path(Investing.com,"BEL6Y.csv")))
Investing.csv.xts(BEL6Y, type = "y")
BEL6Y<-to.daily(BEL6Y)

CZE6Y<-data.frame(read.csv(file.path(Investing.com,"CZE6Y.csv")))
Investing.csv.xts(CZE6Y, type = "y")
CZE6Y<-to.daily(CZE6Y)

FIN6Y<-data.frame(read.csv(file.path(Investing.com,"FIN6Y.csv")))
Investing.csv.xts(FIN6Y, type = "y")
FIN6Y<-to.daily(FIN6Y)

FRA6Y<-data.frame(read.csv(file.path(Investing.com,"FRA6Y.csv")))
Investing.csv.xts(FRA6Y, type = "y")
FRA6Y<-to.daily(FRA6Y)

DEU6Y<-data.frame(read.csv(file.path(Investing.com,"DEU6Y.csv")))
Investing.csv.xts(DEU6Y, type = "y")
DEU6Y<-to.daily(DEU6Y)

IND6Y<-data.frame(read.csv(file.path(Investing.com,"IND6Y.csv")))
Investing.csv.xts(IND6Y, type = "y")
IND6Y<-to.daily(IND6Y)

IRL7Y<-data.frame(read.csv(file.path(Investing.com,"IRL7Y.csv")))
Investing.csv.xts(IRL7Y, type = "y")
IRL7Y<-to.daily(IRL7Y)

IRL6Y<-data.frame(read.csv(file.path(Investing.com,"IRL6Y.csv")))
Investing.csv.xts(IRL6Y, type = "y")
IRL6Y<-to.daily(IRL6Y)

ITA6Y<-data.frame(read.csv(file.path(Investing.com,"ITA6Y.csv")))
Investing.csv.xts(ITA6Y, type = "y")
ITA6Y<-to.daily(ITA6Y)

JPN6Y<-data.frame(read.csv(file.path(Investing.com,"JPN6Y.csv")))
Investing.csv.xts(JPN6Y, type = "y")
JPN6Y<-to.daily(JPN6Y)

KEN6Y<-data.frame(read.csv(file.path(Investing.com,"KEN6Y.csv")))
Investing.csv.xts(KEN6Y, type = "y")
KEN6Y<-to.daily(KEN6Y)

NLD6Y<-data.frame(read.csv(file.path(Investing.com,"NLD6Y.csv")))
Investing.csv.xts(NLD6Y, type = "y")
NLD6Y<-to.daily(NLD6Y)

POL6Y<-data.frame(read.csv(file.path(Investing.com,"POL6Y.csv")))
Investing.csv.xts(POL6Y, type = "y")
POL6Y<-to.daily(POL6Y)

PRT6Y<-data.frame(read.csv(file.path(Investing.com,"PRT6Y.csv")))
Investing.csv.xts(PRT6Y, type = "y")
PRT6Y<-to.daily(PRT6Y)

SRB6Y<-data.frame(read.csv(file.path(Investing.com,"SRB6Y.csv")))
Investing.csv.xts(SRB6Y, type = "y")
SRB6Y<-to.daily(SRB6Y)

SVK6Y<-data.frame(read.csv(file.path(Investing.com,"SVK6Y.csv")))
Investing.csv.xts(SVK6Y, type = "y")
SVK6Y<-to.daily(SVK6Y)

ZAF6Y<-data.frame(read.csv(file.path(Investing.com,"ZAF6Y.csv")))
Investing.csv.xts(ZAF6Y, type = "y")
ZAF6Y<-to.daily(ZAF6Y)

ESP6Y<-data.frame(read.csv(file.path(Investing.com,"ESP6Y.csv")))
Investing.csv.xts(ESP6Y, type = "y")
ESP6Y<-to.daily(ESP6Y)

LKA6Y<-data.frame(read.csv(file.path(Investing.com,"LKA6Y.csv")))
Investing.csv.xts(LKA6Y, type = "y")
LKA6Y<-to.daily(LKA6Y)

CHE6Y<-data.frame(read.csv(file.path(Investing.com,"CHE6Y.csv")))
Investing.csv.xts(CHE6Y, type = "y")
CHE6Y<-to.daily(CHE6Y)

GBR6Y<-data.frame(read.csv(file.path(Investing.com,"GBR6Y.csv")))
Investing.csv.xts(GBR6Y, type = "y")
GBR6Y<-to.daily(GBR6Y)

AUS7Y<-data.frame(read.csv(file.path(Investing.com,"AUS7Y.csv")))
Investing.csv.xts(AUS7Y, type = "y")
AUS7Y<-to.daily(AUS7Y)

AUT7Y<-data.frame(read.csv(file.path(Investing.com,"AUT7Y.csv")))
Investing.csv.xts(AUT7Y, type = "y")
AUT7Y<-to.daily(AUT7Y)

BEL7Y<-data.frame(read.csv(file.path(Investing.com,"BEL7Y.csv")))
Investing.csv.xts(BEL7Y, type = "y")
BEL7Y<-to.daily(BEL7Y)

BWA7Y<-data.frame(read.csv(file.path(Investing.com,"BWA7Y.csv")))
Investing.csv.xts(BWA7Y, type = "y")
BWA7Y<-to.daily(BWA7Y)

BGR7Y<-data.frame(read.csv(file.path(Investing.com,"BGR7Y.csv")))
Investing.csv.xts(BGR7Y, type = "y")
BGR7Y<-to.daily(BGR7Y)

CAN7Y<-data.frame(read.csv(file.path(Investing.com,"CAN7Y.csv")))
Investing.csv.xts(CAN7Y, type = "y")
CAN7Y<-to.daily(CAN7Y)

CHN7Y<-data.frame(read.csv(file.path(Investing.com,"CHN7Y.csv")))
Investing.csv.xts(CHN7Y, type = "y")
CHN7Y<-to.daily(CHN7Y)

CZE7Y<-data.frame(read.csv(file.path(Investing.com,"CZE7Y.csv")))
Investing.csv.xts(CZE7Y, type = "y")
CZE7Y<-to.daily(CZE7Y)

EGY7Y<-data.frame(read.csv(file.path(Investing.com,"EGY7Y.csv")))
Investing.csv.xts(EGY7Y, type = "y")
EGY7Y<-to.daily(EGY7Y)

FRA7Y<-data.frame(read.csv(file.path(Investing.com,"FRA7Y.csv")))
Investing.csv.xts(FRA7Y, type = "y")
FRA7Y<-to.daily(FRA7Y)

DEU7Y<-data.frame(read.csv(file.path(Investing.com,"DEU7Y.csv")))
Investing.csv.xts(DEU7Y, type = "y")
DEU7Y<-to.daily(DEU7Y)

HKG7Y<-data.frame(read.csv(file.path(Investing.com,"HKG7Y.csv")))
Investing.csv.xts(HKG7Y, type = "y")
HKG7Y<-to.daily(HKG7Y)

IND7Y<-data.frame(read.csv(file.path(Investing.com,"IND7Y.csv")))
Investing.csv.xts(IND7Y, type = "y")
IND7Y<-to.daily(IND7Y)

IRL8Y<-data.frame(read.csv(file.path(Investing.com,"IRL8Y.csv")))
Investing.csv.xts(IRL8Y, type = "y")
IRL8Y<-to.daily(IRL8Y)

ITA7Y<-data.frame(read.csv(file.path(Investing.com,"ITA7Y.csv")))
Investing.csv.xts(ITA7Y, type = "y")
ITA7Y<-to.daily(ITA7Y)

JPN7Y<-data.frame(read.csv(file.path(Investing.com,"JPN7Y.csv")))
Investing.csv.xts(JPN7Y, type = "y")
JPN7Y<-to.daily(JPN7Y)

JOR7Y<-data.frame(read.csv(file.path(Investing.com,"JOR7Y.csv")))
Investing.csv.xts(JOR7Y, type = "y")
JOR7Y<-to.daily(JOR7Y)

KEN7Y<-data.frame(read.csv(file.path(Investing.com,"KEN7Y.csv")))
Investing.csv.xts(KEN7Y, type = "y")
KEN7Y<-to.daily(KEN7Y)

MYS7Y<-data.frame(read.csv(file.path(Investing.com,"MYS7Y.csv")))
Investing.csv.xts(MYS7Y, type = "y")
MYS7Y<-to.daily(MYS7Y)

MEX7Y<-data.frame(read.csv(file.path(Investing.com,"MEX7Y.csv")))
Investing.csv.xts(MEX7Y, type = "y")
MEX7Y<-to.daily(MEX7Y)

NAM7Y<-data.frame(read.csv(file.path(Investing.com,"NAM7Y.csv")))
Investing.csv.xts(NAM7Y, type = "y")
NAM7Y<-to.daily(NAM7Y)

NLD7Y<-data.frame(read.csv(file.path(Investing.com,"NLD7Y.csv")))
Investing.csv.xts(NLD7Y, type = "y")
NLD7Y<-to.daily(NLD7Y)

NZL7Y<-data.frame(read.csv(file.path(Investing.com,"NZL7Y.csv")))
Investing.csv.xts(NZL7Y, type = "y")
NZL7Y<-to.daily(NZL7Y)

NGA7Y<-data.frame(read.csv(file.path(Investing.com,"NGA7Y.csv")))
Investing.csv.xts(NGA7Y, type = "y")
NGA7Y<-to.daily(NGA7Y)

PHL7Y<-data.frame(read.csv(file.path(Investing.com,"PHL7Y.csv")))
Investing.csv.xts(PHL7Y, type = "y")
PHL7Y<-to.daily(PHL7Y)

PRT7Y<-data.frame(read.csv(file.path(Investing.com,"PRT7Y.csv")))
Investing.csv.xts(PRT7Y, type = "y")
PRT7Y<-to.daily(PRT7Y)

ROU7Y<-data.frame(read.csv(file.path(Investing.com,"ROU7Y.csv")))
Investing.csv.xts(ROU7Y, type = "y")
ROU7Y<-to.daily(ROU7Y)

RUS7Y<-data.frame(read.csv(file.path(Investing.com,"RUS7Y.csv")))
Investing.csv.xts(RUS7Y, type = "y")
RUS7Y<-to.daily(RUS7Y)

SRB7Y<-data.frame(read.csv(file.path(Investing.com,"SRB7Y.csv")))
Investing.csv.xts(SRB7Y, type = "y")
SRB7Y<-to.daily(SRB7Y)

SVK7Y<-data.frame(read.csv(file.path(Investing.com,"SVK7Y.csv")))
Investing.csv.xts(SVK7Y, type = "y")
SVK7Y<-to.daily(SVK7Y)

SVN7Y<-data.frame(read.csv(file.path(Investing.com,"SVN7Y.csv")))
Investing.csv.xts(SVN7Y, type = "y")
SVN7Y<-to.daily(SVN7Y)

ESP7Y<-data.frame(read.csv(file.path(Investing.com,"ESP7Y.csv")))
Investing.csv.xts(ESP7Y, type = "y")
ESP7Y<-to.daily(ESP7Y)

LKA7Y<-data.frame(read.csv(file.path(Investing.com,"LKA7Y.csv")))
Investing.csv.xts(LKA7Y, type = "y")
LKA7Y<-to.daily(LKA7Y)

SWE7Y<-data.frame(read.csv(file.path(Investing.com,"SWE7Y.csv")))
Investing.csv.xts(SWE7Y, type = "y")
SWE7Y<-to.daily(SWE7Y)

CHE7Y<-data.frame(read.csv(file.path(Investing.com,"CHE7Y.csv")))
Investing.csv.xts(CHE7Y, type = "y")
CHE7Y<-to.daily(CHE7Y)

THA7Y<-data.frame(read.csv(file.path(Investing.com,"THA7Y.csv")))
Investing.csv.xts(THA7Y, type = "y")
THA7Y<-to.daily(THA7Y)

GBR7Y<-data.frame(read.csv(file.path(Investing.com,"GBR7Y.csv")))
Investing.csv.xts(GBR7Y, type = "y")
GBR7Y<-to.daily(GBR7Y)

USA7Y<-data.frame(read.csv(file.path(Investing.com,"USA7Y.csv")))
Investing.csv.xts(USA7Y, type = "y")
USA7Y<-to.daily(USA7Y)

VNM7Y<-data.frame(read.csv(file.path(Investing.com,"VNM7Y.csv")))
Investing.csv.xts(VNM7Y, type = "y")
VNM7Y<-to.daily(VNM7Y)

AUS8Y<-data.frame(read.csv(file.path(Investing.com,"AUS8Y.csv")))
Investing.csv.xts(AUS8Y, type = "y")
AUS8Y<-to.daily(AUS8Y)

AUT8Y<-data.frame(read.csv(file.path(Investing.com,"AUT8Y.csv")))
Investing.csv.xts(AUT8Y, type = "y")
AUT8Y<-to.daily(AUT8Y)

BEL8Y<-data.frame(read.csv(file.path(Investing.com,"BEL8Y.csv")))
Investing.csv.xts(BEL8Y, type = "y")
BEL8Y<-to.daily(BEL8Y)

BRA8Y<-data.frame(read.csv(file.path(Investing.com,"BRA8Y.csv")))
Investing.csv.xts(BRA8Y, type = "y")
BRA8Y<-to.daily(BRA8Y)

CHL8Y<-data.frame(read.csv(file.path(Investing.com,"CHL8Y.csv")))
Investing.csv.xts(CHL8Y, type = "y")
CHL8Y<-to.daily(CHL8Y)

CZE8Y<-data.frame(read.csv(file.path(Investing.com,"CZE8Y.csv")))
Investing.csv.xts(CZE8Y, type = "y")
CZE8Y<-to.daily(CZE8Y)

DNK8Y<-data.frame(read.csv(file.path(Investing.com,"DNK8Y.csv")))
Investing.csv.xts(DNK8Y, type = "y")
DNK8Y<-to.daily(DNK8Y)

FIN8Y<-data.frame(read.csv(file.path(Investing.com,"FIN8Y.csv")))
Investing.csv.xts(FIN8Y, type = "y")
FIN8Y<-to.daily(FIN8Y)

FRA8Y<-data.frame(read.csv(file.path(Investing.com,"FRA8Y.csv")))
Investing.csv.xts(FRA8Y, type = "y")
FRA8Y<-to.daily(FRA8Y)

DEU8Y<-data.frame(read.csv(file.path(Investing.com,"DEU8Y.csv")))
Investing.csv.xts(DEU8Y, type = "y")
DEU8Y<-to.daily(DEU8Y)

IND8Y<-data.frame(read.csv(file.path(Investing.com,"IND8Y.csv")))
Investing.csv.xts(IND8Y, type = "y")
IND8Y<-to.daily(IND8Y)

ITA8Y<-data.frame(read.csv(file.path(Investing.com,"ITA8Y.csv")))
Investing.csv.xts(ITA8Y, type = "y")
ITA8Y<-to.daily(ITA8Y)

JPN8Y<-data.frame(read.csv(file.path(Investing.com,"JPN8Y.csv")))
Investing.csv.xts(JPN8Y, type = "y")
JPN8Y<-to.daily(JPN8Y)

KEN8Y<-data.frame(read.csv(file.path(Investing.com,"KEN8Y.csv")))
Investing.csv.xts(KEN8Y, type = "y")
KEN8Y<-to.daily(KEN8Y)

NLD8Y<-data.frame(read.csv(file.path(Investing.com,"NLD8Y.csv")))
Investing.csv.xts(NLD8Y, type = "y")
NLD8Y<-to.daily(NLD8Y)

POL8Y<-data.frame(read.csv(file.path(Investing.com,"POL8Y.csv")))
Investing.csv.xts(POL8Y, type = "y")
POL8Y<-to.daily(POL8Y)

PRT8Y<-data.frame(read.csv(file.path(Investing.com,"PRT8Y.csv")))
Investing.csv.xts(PRT8Y, type = "y")
PRT8Y<-to.daily(PRT8Y)

SVK8Y<-data.frame(read.csv(file.path(Investing.com,"SVK8Y.csv")))
Investing.csv.xts(SVK8Y, type = "y")
SVK8Y<-to.daily(SVK8Y)

ESP8Y<-data.frame(read.csv(file.path(Investing.com,"ESP8Y.csv")))
Investing.csv.xts(ESP8Y, type = "y")
ESP8Y<-to.daily(ESP8Y)

LKA8Y<-data.frame(read.csv(file.path(Investing.com,"LKA8Y.csv")))
Investing.csv.xts(LKA8Y, type = "y")
LKA8Y<-to.daily(LKA8Y)

CHE8Y<-data.frame(read.csv(file.path(Investing.com,"CHE8Y.csv")))
Investing.csv.xts(CHE8Y, type = "y")
CHE8Y<-to.daily(CHE8Y)

GBR8Y<-data.frame(read.csv(file.path(Investing.com,"GBR8Y.csv")))
Investing.csv.xts(GBR8Y, type = "y")
GBR8Y<-to.daily(GBR8Y)

ARG9Y<-data.frame(read.csv(file.path(Investing.com,"ARG9Y.csv")))
Investing.csv.xts(ARG9Y, type = "y")
ARG9Y<-to.daily(ARG9Y)

AUS9Y<-data.frame(read.csv(file.path(Investing.com,"AUS9Y.csv")))
Investing.csv.xts(AUS9Y, type = "y")
AUS9Y<-to.daily(AUS9Y)

AUT9Y<-data.frame(read.csv(file.path(Investing.com,"AUT9Y.csv")))
Investing.csv.xts(AUT9Y, type = "y")
AUT9Y<-to.daily(AUT9Y)

BEL9Y<-data.frame(read.csv(file.path(Investing.com,"BEL9Y.csv")))
Investing.csv.xts(BEL9Y, type = "y")
BEL9Y<-to.daily(BEL9Y)

CZE9Y<-data.frame(read.csv(file.path(Investing.com,"CZE9Y.csv")))
Investing.csv.xts(CZE9Y, type = "y")
CZE9Y<-to.daily(CZE9Y)

FRA9Y<-data.frame(read.csv(file.path(Investing.com,"FRA9Y.csv")))
Investing.csv.xts(FRA9Y, type = "y")
FRA9Y<-to.daily(FRA9Y)

DEU9Y<-data.frame(read.csv(file.path(Investing.com,"DEU9Y.csv")))
Investing.csv.xts(DEU9Y, type = "y")
DEU9Y<-to.daily(DEU9Y)

IND9Y<-data.frame(read.csv(file.path(Investing.com,"IND9Y.csv")))
Investing.csv.xts(IND9Y, type = "y")
IND9Y<-to.daily(IND9Y)

IRL10Y<-data.frame(read.csv(file.path(Investing.com,"IRL10Y.csv")))
Investing.csv.xts(IRL10Y, type = "y")
IRL10Y<-to.daily(IRL10Y)

ITA9Y<-data.frame(read.csv(file.path(Investing.com,"ITA9Y.csv")))
Investing.csv.xts(ITA9Y, type = "y")
ITA9Y<-to.daily(ITA9Y)

JPN9Y<-data.frame(read.csv(file.path(Investing.com,"JPN9Y.csv")))
Investing.csv.xts(JPN9Y, type = "y")
JPN9Y<-to.daily(JPN9Y)

KEN9Y<-data.frame(read.csv(file.path(Investing.com,"KEN9Y.csv")))
Investing.csv.xts(KEN9Y, type = "y")
KEN9Y<-to.daily(KEN9Y)

NLD9Y<-data.frame(read.csv(file.path(Investing.com,"NLD9Y.csv")))
Investing.csv.xts(NLD9Y, type = "y")
NLD9Y<-to.daily(NLD9Y)

PER9Y<-data.frame(read.csv(file.path(Investing.com,"PER9Y.csv")))
Investing.csv.xts(PER9Y, type = "y")
PER9Y<-to.daily(PER9Y)

POL9Y<-data.frame(read.csv(file.path(Investing.com,"POL9Y.csv")))
Investing.csv.xts(POL9Y, type = "y")
POL9Y<-to.daily(POL9Y)

PRT9Y<-data.frame(read.csv(file.path(Investing.com,"PRT9Y.csv")))
Investing.csv.xts(PRT9Y, type = "y")
PRT9Y<-to.daily(PRT9Y)

SVK9Y<-data.frame(read.csv(file.path(Investing.com,"SVK9Y.csv")))
Investing.csv.xts(SVK9Y, type = "y")
SVK9Y<-to.daily(SVK9Y)

SVN9Y<-data.frame(read.csv(file.path(Investing.com,"SVN9Y.csv")))
Investing.csv.xts(SVN9Y, type = "y")
SVN9Y<-to.daily(SVN9Y)

ESP9Y<-data.frame(read.csv(file.path(Investing.com,"ESP9Y.csv")))
Investing.csv.xts(ESP9Y, type = "y")
ESP9Y<-to.daily(ESP9Y)

LKA9Y<-data.frame(read.csv(file.path(Investing.com,"LKA9Y.csv")))
Investing.csv.xts(LKA9Y, type = "y")
LKA9Y<-to.daily(LKA9Y)

CHE9Y<-data.frame(read.csv(file.path(Investing.com,"CHE9Y.csv")))
Investing.csv.xts(CHE9Y, type = "y")
CHE9Y<-to.daily(CHE9Y)

GBR9Y<-data.frame(read.csv(file.path(Investing.com,"GBR9Y.csv")))
Investing.csv.xts(GBR9Y, type = "y")
GBR9Y<-to.daily(GBR9Y)

AUS10Y<-data.frame(read.csv(file.path(Investing.com,"AUS10Y.csv")))
Investing.csv.xts(AUS10Y, type = "y")
AUS10Y<-to.daily(AUS10Y)

AUT10Y<-data.frame(read.csv(file.path(Investing.com,"AUT10Y.csv")))
Investing.csv.xts(AUT10Y, type = "y")
AUT10Y<-to.daily(AUT10Y)

BGD10Y<-data.frame(read.csv(file.path(Investing.com,"BGD10Y.csv")))
Investing.csv.xts(BGD10Y, type = "y")
BGD10Y<-to.daily(BGD10Y)

BEL10Y<-data.frame(read.csv(file.path(Investing.com,"BEL10Y.csv")))
Investing.csv.xts(BEL10Y, type = "y")
BEL10Y<-to.daily(BEL10Y)

BRA10Y<-data.frame(read.csv(file.path(Investing.com,"BRA10Y.csv")))
Investing.csv.xts(BRA10Y, type = "y")
BRA10Y<-to.daily(BRA10Y)

BGR10Y<-data.frame(read.csv(file.path(Investing.com,"BGR10Y.csv")))
Investing.csv.xts(BGR10Y, type = "y")
BGR10Y<-to.daily(BGR10Y)

CAN10Y<-data.frame(read.csv(file.path(Investing.com,"CAN10Y.csv")))
Investing.csv.xts(CAN10Y, type = "y")
CAN10Y<-to.daily(CAN10Y)

CHL10Y<-data.frame(read.csv(file.path(Investing.com,"CHL10Y.csv")))
Investing.csv.xts(CHL10Y, type = "y")
CHL10Y<-to.daily(CHL10Y)

CHN10Y<-data.frame(read.csv(file.path(Investing.com,"CHN10Y.csv")))
Investing.csv.xts(CHN10Y, type = "y")
CHN10Y<-to.daily(CHN10Y)

COL10Y<-data.frame(read.csv(file.path(Investing.com,"COL10Y.csv")))
Investing.csv.xts(COL10Y, type = "y")
COL10Y<-to.daily(COL10Y)

HRV10Y<-data.frame(read.csv(file.path(Investing.com,"HRV10Y.csv")))
Investing.csv.xts(HRV10Y, type = "y")
HRV10Y<-to.daily(HRV10Y)

CZE10Y<-data.frame(read.csv(file.path(Investing.com,"CZE10Y.csv")))
Investing.csv.xts(CZE10Y, type = "y")
CZE10Y<-to.daily(CZE10Y)

DNK10Y<-data.frame(read.csv(file.path(Investing.com,"DNK10Y.csv")))
Investing.csv.xts(DNK10Y, type = "y")
DNK10Y<-to.daily(DNK10Y)

EGY10Y<-data.frame(read.csv(file.path(Investing.com,"EGY10Y.csv")))
Investing.csv.xts(EGY10Y, type = "y")
EGY10Y<-to.daily(EGY10Y)

FIN10Y<-data.frame(read.csv(file.path(Investing.com,"FIN10Y.csv")))
Investing.csv.xts(FIN10Y, type = "y")
FIN10Y<-to.daily(FIN10Y)

FRA10Y<-data.frame(read.csv(file.path(Investing.com,"FRA10Y.csv")))
Investing.csv.xts(FRA10Y, type = "y")
FRA10Y<-to.daily(FRA10Y)

DEU10Y<-data.frame(read.csv(file.path(Investing.com,"DEU10Y.csv")))
Investing.csv.xts(DEU10Y, type = "y")
DEU10Y<-to.daily(DEU10Y)

GRC10Y<-data.frame(read.csv(file.path(Investing.com,"GRC10Y.csv")))
Investing.csv.xts(GRC10Y, type = "y")
GRC10Y<-to.daily(GRC10Y)

HKG10Y<-data.frame(read.csv(file.path(Investing.com,"HKG10Y.csv")))
Investing.csv.xts(HKG10Y, type = "y")
HKG10Y<-to.daily(HKG10Y)

HUN10Y<-data.frame(read.csv(file.path(Investing.com,"HUN10Y.csv")))
Investing.csv.xts(HUN10Y, type = "y")
HUN10Y<-to.daily(HUN10Y)

ISL10Y<-data.frame(read.csv(file.path(Investing.com,"ISL10Y.csv")))
Investing.csv.xts(ISL10Y, type = "y")
ISL10Y<-to.daily(ISL10Y)

IND10Y<-data.frame(read.csv(file.path(Investing.com,"IND10Y.csv")))
Investing.csv.xts(IND10Y, type = "y")
IND10Y<-to.daily(IND10Y)

IDN10Y<-data.frame(read.csv(file.path(Investing.com,"IDN10Y.csv")))
Investing.csv.xts(IDN10Y, type = "y")
IDN10Y<-to.daily(IDN10Y)

ISR10Y<-data.frame(read.csv(file.path(Investing.com,"ISR10Y.csv")))
Investing.csv.xts(ISR10Y, type = "y")
ISR10Y<-to.daily(ISR10Y)

ITA10Y<-data.frame(read.csv(file.path(Investing.com,"ITA10Y.csv")))
Investing.csv.xts(ITA10Y, type = "y")
ITA10Y<-to.daily(ITA10Y)

JPN10Y<-data.frame(read.csv(file.path(Investing.com,"JPN10Y.csv")))
Investing.csv.xts(JPN10Y, type = "y")
JPN10Y<-to.daily(JPN10Y)

JOR10Y<-data.frame(read.csv(file.path(Investing.com,"JOR10Y.csv")))
Investing.csv.xts(JOR10Y, type = "y")
JOR10Y<-to.daily(JOR10Y)

KEN10Y<-data.frame(read.csv(file.path(Investing.com,"KEN10Y.csv")))
Investing.csv.xts(KEN10Y, type = "y")
KEN10Y<-to.daily(KEN10Y)

LTU10Y<-data.frame(read.csv(file.path(Investing.com,"LTU10Y.csv")))
Investing.csv.xts(LTU10Y, type = "y")
LTU10Y<-to.daily(LTU10Y)

MYS10Y<-data.frame(read.csv(file.path(Investing.com,"MYS10Y.csv")))
Investing.csv.xts(MYS10Y, type = "y")
MYS10Y<-to.daily(MYS10Y)

MLT10Y<-data.frame(read.csv(file.path(Investing.com,"MLT10Y.csv")))
Investing.csv.xts(MLT10Y, type = "y")
MLT10Y<-to.daily(MLT10Y)

MUS10Y<-data.frame(read.csv(file.path(Investing.com,"MUS10Y.csv")))
Investing.csv.xts(MUS10Y, type = "y")
MUS10Y<-to.daily(MUS10Y)

MEX10Y<-data.frame(read.csv(file.path(Investing.com,"MEX10Y.csv")))
Investing.csv.xts(MEX10Y, type = "y")
MEX10Y<-to.daily(MEX10Y)

MAR10Y<-data.frame(read.csv(file.path(Investing.com,"MAR10Y.csv")))
Investing.csv.xts(MAR10Y, type = "y")
MAR10Y<-to.daily(MAR10Y)

NAM10Y<-data.frame(read.csv(file.path(Investing.com,"NAM10Y.csv")))
Investing.csv.xts(NAM10Y, type = "y")
NAM10Y<-to.daily(NAM10Y)

NLD10Y<-data.frame(read.csv(file.path(Investing.com,"NLD10Y.csv")))
Investing.csv.xts(NLD10Y, type = "y")
NLD10Y<-to.daily(NLD10Y)

NZL10Y<-data.frame(read.csv(file.path(Investing.com,"NZL10Y.csv")))
Investing.csv.xts(NZL10Y, type = "y")
NZL10Y<-to.daily(NZL10Y)

NGA10Y<-data.frame(read.csv(file.path(Investing.com,"NGA10Y.csv")))
Investing.csv.xts(NGA10Y, type = "y")
NGA10Y<-to.daily(NGA10Y)

NOR10Y<-data.frame(read.csv(file.path(Investing.com,"NOR10Y.csv")))
Investing.csv.xts(NOR10Y, type = "y")
NOR10Y<-to.daily(NOR10Y)

PAK10Y<-data.frame(read.csv(file.path(Investing.com,"PAK10Y.csv")))
Investing.csv.xts(PAK10Y, type = "y")
PAK10Y<-to.daily(PAK10Y)

PHL10Y<-data.frame(read.csv(file.path(Investing.com,"PHL10Y.csv")))
Investing.csv.xts(PHL10Y, type = "y")
PHL10Y<-to.daily(PHL10Y)

POL10Y<-data.frame(read.csv(file.path(Investing.com,"POL10Y.csv")))
Investing.csv.xts(POL10Y, type = "y")
POL10Y<-to.daily(POL10Y)

PRT10Y<-data.frame(read.csv(file.path(Investing.com,"PRT10Y.csv")))
Investing.csv.xts(PRT10Y, type = "y")
PRT10Y<-to.daily(PRT10Y)

QAT10Y<-data.frame(read.csv(file.path(Investing.com,"QAT10Y.csv")))
Investing.csv.xts(QAT10Y, type = "y")
QAT10Y<-to.daily(QAT10Y)

ROU10Y<-data.frame(read.csv(file.path(Investing.com,"ROU10Y.csv")))
Investing.csv.xts(ROU10Y, type = "y")
ROU10Y<-to.daily(ROU10Y)

RUS10Y<-data.frame(read.csv(file.path(Investing.com,"RUS10Y.csv")))
Investing.csv.xts(RUS10Y, type = "y")
RUS10Y<-to.daily(RUS10Y)

SGP10Y<-data.frame(read.csv(file.path(Investing.com,"SGP10Y.csv")))
Investing.csv.xts(SGP10Y, type = "y")
SGP10Y<-to.daily(SGP10Y)

SVK10Y<-data.frame(read.csv(file.path(Investing.com,"SVK10Y.csv")))
Investing.csv.xts(SVK10Y, type = "y")
SVK10Y<-to.daily(SVK10Y)

SVN10Y<-data.frame(read.csv(file.path(Investing.com,"SVN10Y.csv")))
Investing.csv.xts(SVN10Y, type = "y")
SVN10Y<-to.daily(SVN10Y)

ZAF10Y<-data.frame(read.csv(file.path(Investing.com,"ZAF10Y.csv")))
Investing.csv.xts(ZAF10Y, type = "y")
ZAF10Y<-to.daily(ZAF10Y)

KOR10Y<-data.frame(read.csv(file.path(Investing.com,"KOR10Y.csv")))
Investing.csv.xts(KOR10Y, type = "y")
KOR10Y<-to.daily(KOR10Y)

ESP10Y<-data.frame(read.csv(file.path(Investing.com,"ESP10Y.csv")))
Investing.csv.xts(ESP10Y, type = "y")
ESP10Y<-to.daily(ESP10Y)

LKA10Y<-data.frame(read.csv(file.path(Investing.com,"LKA10Y.csv")))
Investing.csv.xts(LKA10Y, type = "y")
LKA10Y<-to.daily(LKA10Y)

SWE10Y<-data.frame(read.csv(file.path(Investing.com,"SWE10Y.csv")))
Investing.csv.xts(SWE10Y, type = "y")
SWE10Y<-to.daily(SWE10Y)

CHE10Y<-data.frame(read.csv(file.path(Investing.com,"CHE10Y.csv")))
Investing.csv.xts(CHE10Y, type = "y")
CHE10Y<-to.daily(CHE10Y)

TWN10Y<-data.frame(read.csv(file.path(Investing.com,"TWN10Y.csv")))
Investing.csv.xts(TWN10Y, type = "y")
TWN10Y<-to.daily(TWN10Y)

THA10Y<-data.frame(read.csv(file.path(Investing.com,"THA10Y.csv")))
Investing.csv.xts(THA10Y, type = "y")
THA10Y<-to.daily(THA10Y)

TUR10Y<-data.frame(read.csv(file.path(Investing.com,"TUR10Y.csv")))
Investing.csv.xts(TUR10Y, type = "y")
TUR10Y<-to.daily(TUR10Y)

UGA10Y<-data.frame(read.csv(file.path(Investing.com,"UGA10Y.csv")))
Investing.csv.xts(UGA10Y, type = "y")
UGA10Y<-to.daily(UGA10Y)

GBR10Y<-data.frame(read.csv(file.path(Investing.com,"GBR10Y.csv")))
Investing.csv.xts(GBR10Y, type = "y")
GBR10Y<-to.daily(GBR10Y)

USA10Y<-data.frame(read.csv(file.path(Investing.com,"USA10Y.csv")))
Investing.csv.xts(USA10Y, type = "y")
USA10Y<-to.daily(USA10Y)

VNM10Y<-data.frame(read.csv(file.path(Investing.com,"VNM10Y.csv")))
Investing.csv.xts(VNM10Y, type = "y")
VNM10Y<-to.daily(VNM10Y)

IND11Y<-data.frame(read.csv(file.path(Investing.com,"IND11Y.csv")))
Investing.csv.xts(IND11Y, type = "y")
IND11Y<-to.daily(IND11Y)

AUS12Y<-data.frame(read.csv(file.path(Investing.com,"AUS12Y.csv")))
Investing.csv.xts(AUS12Y, type = "y")
AUS12Y<-to.daily(AUS12Y)

IND12Y<-data.frame(read.csv(file.path(Investing.com,"IND12Y.csv")))
Investing.csv.xts(IND12Y, type = "y")
IND12Y<-to.daily(IND12Y)

KEN12Y<-data.frame(read.csv(file.path(Investing.com,"KEN12Y.csv")))
Investing.csv.xts(KEN12Y, type = "y")
KEN12Y<-to.daily(KEN12Y)

POL12Y<-data.frame(read.csv(file.path(Investing.com,"POL12Y.csv")))
Investing.csv.xts(POL12Y, type = "y")
POL12Y<-to.daily(POL12Y)

SVK12Y<-data.frame(read.csv(file.path(Investing.com,"SVK12Y.csv")))
Investing.csv.xts(SVK12Y, type = "y")
SVK12Y<-to.daily(SVK12Y)

THA12Y<-data.frame(read.csv(file.path(Investing.com,"THA12Y.csv")))
Investing.csv.xts(THA12Y, type = "y")
THA12Y<-to.daily(THA12Y)

GBR12Y<-data.frame(read.csv(file.path(Investing.com,"GBR12Y.csv")))
Investing.csv.xts(GBR12Y, type = "y")
GBR12Y<-to.daily(GBR12Y)

BWA13Y<-data.frame(read.csv(file.path(Investing.com,"BWA13Y.csv")))
Investing.csv.xts(BWA13Y, type = "y")
BWA13Y<-to.daily(BWA13Y)

IND13Y<-data.frame(read.csv(file.path(Investing.com,"IND13Y.csv")))
Investing.csv.xts(IND13Y, type = "y")
IND13Y<-to.daily(IND13Y)

IND14Y<-data.frame(read.csv(file.path(Investing.com,"IND14Y.csv")))
Investing.csv.xts(IND14Y, type = "y")
IND14Y<-to.daily(IND14Y)

PAK14Y<-data.frame(read.csv(file.path(Investing.com,"PAK14Y.csv")))
Investing.csv.xts(PAK14Y, type = "y")
PAK14Y<-to.daily(PAK14Y)

IRL15Y<-data.frame(read.csv(file.path(Investing.com,"IRL15Y.csv")))
Investing.csv.xts(IRL15Y, type = "y")
IRL15Y<-to.daily(IRL15Y)

SVK14Y<-data.frame(read.csv(file.path(Investing.com,"SVK14Y.csv")))
Investing.csv.xts(SVK14Y, type = "y")
SVK14Y<-to.daily(SVK14Y)

THA14Y<-data.frame(read.csv(file.path(Investing.com,"THA14Y.csv")))
Investing.csv.xts(THA14Y, type = "y")
THA14Y<-to.daily(THA14Y)

AUS15Y<-data.frame(read.csv(file.path(Investing.com,"AUS15Y.csv")))
Investing.csv.xts(AUS15Y, type = "y")
AUS15Y<-to.daily(AUS15Y)

AUT15Y<-data.frame(read.csv(file.path(Investing.com,"AUT15Y.csv")))
Investing.csv.xts(AUT15Y, type = "y")
AUT15Y<-to.daily(AUT15Y)

BGD15Y<-data.frame(read.csv(file.path(Investing.com,"BGD15Y.csv")))
Investing.csv.xts(BGD15Y, type = "y")
BGD15Y<-to.daily(BGD15Y)

BEL15Y<-data.frame(read.csv(file.path(Investing.com,"BEL15Y.csv")))
Investing.csv.xts(BEL15Y, type = "y")
BEL15Y<-to.daily(BEL15Y)

CHN15Y<-data.frame(read.csv(file.path(Investing.com,"CHN15Y.csv")))
Investing.csv.xts(CHN15Y, type = "y")
CHN15Y<-to.daily(CHN15Y)

COL15Y<-data.frame(read.csv(file.path(Investing.com,"COL15Y.csv")))
Investing.csv.xts(COL15Y, type = "y")
COL15Y<-to.daily(COL15Y)

CZE15Y<-data.frame(read.csv(file.path(Investing.com,"CZE15Y.csv")))
Investing.csv.xts(CZE15Y, type = "y")
CZE15Y<-to.daily(CZE15Y)

FIN15Y<-data.frame(read.csv(file.path(Investing.com,"FIN15Y.csv")))
Investing.csv.xts(FIN15Y, type = "y")
FIN15Y<-to.daily(FIN15Y)

FRA15Y<-data.frame(read.csv(file.path(Investing.com,"FRA15Y.csv")))
Investing.csv.xts(FRA15Y, type = "y")
FRA15Y<-to.daily(FRA15Y)

DEU15Y<-data.frame(read.csv(file.path(Investing.com,"DEU15Y.csv")))
Investing.csv.xts(DEU15Y, type = "y")
DEU15Y<-to.daily(DEU15Y)

GRC15Y<-data.frame(read.csv(file.path(Investing.com,"GRC15Y.csv")))
Investing.csv.xts(GRC15Y, type = "y")
GRC15Y<-to.daily(GRC15Y)

HKG15Y<-data.frame(read.csv(file.path(Investing.com,"HKG15Y.csv")))
Investing.csv.xts(HKG15Y, type = "y")
HKG15Y<-to.daily(HKG15Y)

HUN15Y<-data.frame(read.csv(file.path(Investing.com,"HUN15Y.csv")))
Investing.csv.xts(HUN15Y, type = "y")
HUN15Y<-to.daily(HUN15Y)

IND15Y<-data.frame(read.csv(file.path(Investing.com,"IND15Y.csv")))
Investing.csv.xts(IND15Y, type = "y")
IND15Y<-to.daily(IND15Y)

IDN15Y<-data.frame(read.csv(file.path(Investing.com,"IDN15Y.csv")))
Investing.csv.xts(IDN15Y, type = "y")
IDN15Y<-to.daily(IDN15Y)

ITA15Y<-data.frame(read.csv(file.path(Investing.com,"ITA15Y.csv")))
Investing.csv.xts(ITA15Y, type = "y")
ITA15Y<-to.daily(ITA15Y)

JPN15Y<-data.frame(read.csv(file.path(Investing.com,"JPN15Y.csv")))
Investing.csv.xts(JPN15Y, type = "y")
JPN15Y<-to.daily(JPN15Y)

KEN15Y<-data.frame(read.csv(file.path(Investing.com,"KEN15Y.csv")))
Investing.csv.xts(KEN15Y, type = "y")
KEN15Y<-to.daily(KEN15Y)

MYS15Y<-data.frame(read.csv(file.path(Investing.com,"MYS15Y.csv")))
Investing.csv.xts(MYS15Y, type = "y")
MYS15Y<-to.daily(MYS15Y)

MUS15Y<-data.frame(read.csv(file.path(Investing.com,"MUS15Y.csv")))
Investing.csv.xts(MUS15Y, type = "y")
MUS15Y<-to.daily(MUS15Y)

MEX15Y<-data.frame(read.csv(file.path(Investing.com,"MEX15Y.csv")))
Investing.csv.xts(MEX15Y, type = "y")
MEX15Y<-to.daily(MEX15Y)

MAR15Y<-data.frame(read.csv(file.path(Investing.com,"MAR15Y.csv")))
Investing.csv.xts(MAR15Y, type = "y")
MAR15Y<-to.daily(MAR15Y)

NAM15Y<-data.frame(read.csv(file.path(Investing.com,"NAM15Y.csv")))
Investing.csv.xts(NAM15Y, type = "y")
NAM15Y<-to.daily(NAM15Y)

NLD15Y<-data.frame(read.csv(file.path(Investing.com,"NLD15Y.csv")))
Investing.csv.xts(NLD15Y, type = "y")
NLD15Y<-to.daily(NLD15Y)

NZL15Y<-data.frame(read.csv(file.path(Investing.com,"NZL15Y.csv")))
Investing.csv.xts(NZL15Y, type = "y")
NZL15Y<-to.daily(NZL15Y)

NGA15Y<-data.frame(read.csv(file.path(Investing.com,"NGA15Y.csv")))
Investing.csv.xts(NGA15Y, type = "y")
NGA15Y<-to.daily(NGA15Y)

PER15Y<-data.frame(read.csv(file.path(Investing.com,"PER15Y.csv")))
Investing.csv.xts(PER15Y, type = "y")
PER15Y<-to.daily(PER15Y)

PRT15Y<-data.frame(read.csv(file.path(Investing.com,"PRT15Y.csv")))
Investing.csv.xts(PRT15Y, type = "y")
PRT15Y<-to.daily(PRT15Y)

QAT15Y<-data.frame(read.csv(file.path(Investing.com,"QAT15Y.csv")))
Investing.csv.xts(QAT15Y, type = "y")
QAT15Y<-to.daily(QAT15Y)

RUS15Y<-data.frame(read.csv(file.path(Investing.com,"RUS15Y.csv")))
Investing.csv.xts(RUS15Y, type = "y")
RUS15Y<-to.daily(RUS15Y)

SGP15Y<-data.frame(read.csv(file.path(Investing.com,"SGP15Y.csv")))
Investing.csv.xts(SGP15Y, type = "y")
SGP15Y<-to.daily(SGP15Y)

ZAF15Y<-data.frame(read.csv(file.path(Investing.com,"ZAF15Y.csv")))
Investing.csv.xts(ZAF15Y, type = "y")
ZAF15Y<-to.daily(ZAF15Y)

ESP15Y<-data.frame(read.csv(file.path(Investing.com,"ESP15Y.csv")))
Investing.csv.xts(ESP15Y, type = "y")
ESP15Y<-to.daily(ESP15Y)

LKA15Y<-data.frame(read.csv(file.path(Investing.com,"LKA15Y.csv")))
Investing.csv.xts(LKA15Y, type = "y")
LKA15Y<-to.daily(LKA15Y)

SWE15Y<-data.frame(read.csv(file.path(Investing.com,"SWE15Y.csv")))
Investing.csv.xts(SWE15Y, type = "y")
SWE15Y<-to.daily(SWE15Y)

CHE15Y<-data.frame(read.csv(file.path(Investing.com,"CHE15Y.csv")))
Investing.csv.xts(CHE15Y, type = "y")
CHE15Y<-to.daily(CHE15Y)

THA15Y<-data.frame(read.csv(file.path(Investing.com,"THA15Y.csv")))
Investing.csv.xts(THA15Y, type = "y")
THA15Y<-to.daily(THA15Y)

UGA15Y<-data.frame(read.csv(file.path(Investing.com,"UGA15Y.csv")))
Investing.csv.xts(UGA15Y, type = "y")
UGA15Y<-to.daily(UGA15Y)

GBR15Y<-data.frame(read.csv(file.path(Investing.com,"GBR15Y.csv")))
Investing.csv.xts(GBR15Y, type = "y")
GBR15Y<-to.daily(GBR15Y)

VEN15Y<-data.frame(read.csv(file.path(Investing.com,"VEN15Y.csv")))
Investing.csv.xts(VEN15Y, type = "y")
VEN15Y<-to.daily(VEN15Y)

THA16Y<-data.frame(read.csv(file.path(Investing.com,"THA16Y.csv")))
Investing.csv.xts(THA16Y, type = "y")
THA16Y<-to.daily(THA16Y)

IND19Y<-data.frame(read.csv(file.path(Investing.com,"IND19Y.csv")))
Investing.csv.xts(IND19Y, type = "y")
IND19Y<-to.daily(IND19Y)

IRL20Y<-data.frame(read.csv(file.path(Investing.com,"IRL20Y.csv")))
Investing.csv.xts(IRL20Y, type = "y")
IRL20Y<-to.daily(IRL20Y)

AUS20Y<-data.frame(read.csv(file.path(Investing.com,"AUS20Y.csv")))
Investing.csv.xts(AUS20Y, type = "y")
AUS20Y<-to.daily(AUS20Y)

AUT20Y<-data.frame(read.csv(file.path(Investing.com,"AUT20Y.csv")))
Investing.csv.xts(AUT20Y, type = "y")
AUT20Y<-to.daily(AUT20Y)

BGD20Y<-data.frame(read.csv(file.path(Investing.com,"BGD20Y.csv")))
Investing.csv.xts(BGD20Y, type = "y")
BGD20Y<-to.daily(BGD20Y)

BEL20Y<-data.frame(read.csv(file.path(Investing.com,"BEL20Y.csv")))
Investing.csv.xts(BEL20Y, type = "y")
BEL20Y<-to.daily(BEL20Y)

CAN20Y<-data.frame(read.csv(file.path(Investing.com,"CAN20Y.csv")))
Investing.csv.xts(CAN20Y, type = "y")
CAN20Y<-to.daily(CAN20Y)

CHN20Y<-data.frame(read.csv(file.path(Investing.com,"CHN20Y.csv")))
Investing.csv.xts(CHN20Y, type = "y")
CHN20Y<-to.daily(CHN20Y)

CZE20Y<-data.frame(read.csv(file.path(Investing.com,"CZE20Y.csv")))
Investing.csv.xts(CZE20Y, type = "y")
CZE20Y<-to.daily(CZE20Y)

FRA20Y<-data.frame(read.csv(file.path(Investing.com,"FRA20Y.csv")))
Investing.csv.xts(FRA20Y, type = "y")
FRA20Y<-to.daily(FRA20Y)

DEU20Y<-data.frame(read.csv(file.path(Investing.com,"DEU20Y.csv")))
Investing.csv.xts(DEU20Y, type = "y")
DEU20Y<-to.daily(DEU20Y)

GRC20Y<-data.frame(read.csv(file.path(Investing.com,"GRC20Y.csv")))
Investing.csv.xts(GRC20Y, type = "y")
GRC20Y<-to.daily(GRC20Y)

IDN20Y<-data.frame(read.csv(file.path(Investing.com,"IDN20Y.csv")))
Investing.csv.xts(IDN20Y, type = "y")
IDN20Y<-to.daily(IDN20Y)

ITA20Y<-data.frame(read.csv(file.path(Investing.com,"ITA20Y.csv")))
Investing.csv.xts(ITA20Y, type = "y")
ITA20Y<-to.daily(ITA20Y)

JPN20Y<-data.frame(read.csv(file.path(Investing.com,"JPN20Y.csv")))
Investing.csv.xts(JPN20Y, type = "y")
JPN20Y<-to.daily(JPN20Y)

KEN20Y<-data.frame(read.csv(file.path(Investing.com,"KEN20Y.csv")))
Investing.csv.xts(KEN20Y, type = "y")
KEN20Y<-to.daily(KEN20Y)

MYS20Y<-data.frame(read.csv(file.path(Investing.com,"MYS20Y.csv")))
Investing.csv.xts(MYS20Y, type = "y")
MYS20Y<-to.daily(MYS20Y)

MLT20Y<-data.frame(read.csv(file.path(Investing.com,"MLT20Y.csv")))
Investing.csv.xts(MLT20Y, type = "y")
MLT20Y<-to.daily(MLT20Y)

MUS20Y<-data.frame(read.csv(file.path(Investing.com,"MUS20Y.csv")))
Investing.csv.xts(MUS20Y, type = "y")
MUS20Y<-to.daily(MUS20Y)

MEX20Y<-data.frame(read.csv(file.path(Investing.com,"MEX20Y.csv")))
Investing.csv.xts(MEX20Y, type = "y")
MEX20Y<-to.daily(MEX20Y)

NAM20Y<-data.frame(read.csv(file.path(Investing.com,"NAM20Y.csv")))
Investing.csv.xts(NAM20Y, type = "y")
NAM20Y<-to.daily(NAM20Y)

NLD20Y<-data.frame(read.csv(file.path(Investing.com,"NLD20Y.csv")))
Investing.csv.xts(NLD20Y, type = "y")
NLD20Y<-to.daily(NLD20Y)

NZL20Y<-data.frame(read.csv(file.path(Investing.com,"NZL20Y.csv")))
Investing.csv.xts(NZL20Y, type = "y")
NZL20Y<-to.daily(NZL20Y)

NGA20Y<-data.frame(read.csv(file.path(Investing.com,"NGA20Y.csv")))
Investing.csv.xts(NGA20Y, type = "y")
NGA20Y<-to.daily(NGA20Y)

PAK20Y<-data.frame(read.csv(file.path(Investing.com,"PAK20Y.csv")))
Investing.csv.xts(PAK20Y, type = "y")
PAK20Y<-to.daily(PAK20Y)

PER20Y<-data.frame(read.csv(file.path(Investing.com,"PER20Y.csv")))
Investing.csv.xts(PER20Y, type = "y")
PER20Y<-to.daily(PER20Y)

PHL20Y<-data.frame(read.csv(file.path(Investing.com,"PHL20Y.csv")))
Investing.csv.xts(PHL20Y, type = "y")
PHL20Y<-to.daily(PHL20Y)

PRT20Y<-data.frame(read.csv(file.path(Investing.com,"PRT20Y.csv")))
Investing.csv.xts(PRT20Y, type = "y")
PRT20Y<-to.daily(PRT20Y)

RUS20Y<-data.frame(read.csv(file.path(Investing.com,"RUS20Y.csv")))
Investing.csv.xts(RUS20Y, type = "y")
RUS20Y<-to.daily(RUS20Y)

SGP20Y<-data.frame(read.csv(file.path(Investing.com,"SGP20Y.csv")))
Investing.csv.xts(SGP20Y, type = "y")
SGP20Y<-to.daily(SGP20Y)

SVK20Y<-data.frame(read.csv(file.path(Investing.com,"SVK20Y.csv")))
Investing.csv.xts(SVK20Y, type = "y")
SVK20Y<-to.daily(SVK20Y)

ZAF20Y<-data.frame(read.csv(file.path(Investing.com,"ZAF20Y.csv")))
Investing.csv.xts(ZAF20Y, type = "y")
ZAF20Y<-to.daily(ZAF20Y)

KOR20Y<-data.frame(read.csv(file.path(Investing.com,"KOR20Y.csv")))
Investing.csv.xts(KOR20Y, type = "y")
KOR20Y<-to.daily(KOR20Y)

ESP20Y<-data.frame(read.csv(file.path(Investing.com,"ESP20Y.csv")))
Investing.csv.xts(ESP20Y, type = "y")
ESP20Y<-to.daily(ESP20Y)

SWE20Y<-data.frame(read.csv(file.path(Investing.com,"SWE20Y.csv")))
Investing.csv.xts(SWE20Y, type = "y")
SWE20Y<-to.daily(SWE20Y)

CHE20Y<-data.frame(read.csv(file.path(Investing.com,"CHE20Y.csv")))
Investing.csv.xts(CHE20Y, type = "y")
CHE20Y<-to.daily(CHE20Y)

TWN20Y<-data.frame(read.csv(file.path(Investing.com,"TWN20Y.csv")))
Investing.csv.xts(TWN20Y, type = "y")
TWN20Y<-to.daily(TWN20Y)

THA20Y<-data.frame(read.csv(file.path(Investing.com,"THA20Y.csv")))
Investing.csv.xts(THA20Y, type = "y")
THA20Y<-to.daily(THA20Y)

GBR20Y<-data.frame(read.csv(file.path(Investing.com,"GBR20Y.csv")))
Investing.csv.xts(GBR20Y, type = "y")
GBR20Y<-to.daily(GBR20Y)

VEN20Y<-data.frame(read.csv(file.path(Investing.com,"VEN20Y.csv")))
Investing.csv.xts(VEN20Y, type = "y")
VEN20Y<-to.daily(VEN20Y)

IND24Y<-data.frame(read.csv(file.path(Investing.com,"IND24Y.csv")))
Investing.csv.xts(IND24Y, type = "y")
IND24Y<-to.daily(IND24Y)

AUT25Y<-data.frame(read.csv(file.path(Investing.com,"AUT25Y.csv")))
Investing.csv.xts(AUT25Y, type = "y")
AUT25Y<-to.daily(AUT25Y)

FRA25Y<-data.frame(read.csv(file.path(Investing.com,"FRA25Y.csv")))
Investing.csv.xts(FRA25Y, type = "y")
FRA25Y<-to.daily(FRA25Y)

DEU25Y<-data.frame(read.csv(file.path(Investing.com,"DEU25Y.csv")))
Investing.csv.xts(DEU25Y, type = "y")
DEU25Y<-to.daily(DEU25Y)

GRC25Y<-data.frame(read.csv(file.path(Investing.com,"GRC25Y.csv")))
Investing.csv.xts(GRC25Y, type = "y")
GRC25Y<-to.daily(GRC25Y)

IDN25Y<-data.frame(read.csv(file.path(Investing.com,"IDN25Y.csv")))
Investing.csv.xts(IDN25Y, type = "y")
IDN25Y<-to.daily(IDN25Y)

IRL30Y<-data.frame(read.csv(file.path(Investing.com,"IRL30Y.csv")))
Investing.csv.xts(IRL30Y, type = "y")
IRL30Y<-to.daily(IRL30Y)

KEN25Y<-data.frame(read.csv(file.path(Investing.com,"KEN25Y.csv")))
Investing.csv.xts(KEN25Y, type = "y")
KEN25Y<-to.daily(KEN25Y)

MLT25Y<-data.frame(read.csv(file.path(Investing.com,"MLT25Y.csv")))
Investing.csv.xts(MLT25Y, type = "y")
MLT25Y<-to.daily(MLT25Y)

NLD25Y<-data.frame(read.csv(file.path(Investing.com,"NLD25Y.csv")))
Investing.csv.xts(NLD25Y, type = "y")
NLD25Y<-to.daily(NLD25Y)

PHL25Y<-data.frame(read.csv(file.path(Investing.com,"PHL25Y.csv")))
Investing.csv.xts(PHL25Y, type = "y")
PHL25Y<-to.daily(PHL25Y)

ZAF25Y<-data.frame(read.csv(file.path(Investing.com,"ZAF25Y.csv")))
Investing.csv.xts(ZAF25Y, type = "y")
ZAF25Y<-to.daily(ZAF25Y)

ESP25Y<-data.frame(read.csv(file.path(Investing.com,"ESP25Y.csv")))
Investing.csv.xts(ESP25Y, type = "y")
ESP25Y<-to.daily(ESP25Y)

GBR25Y<-data.frame(read.csv(file.path(Investing.com,"GBR25Y.csv")))
Investing.csv.xts(GBR25Y, type = "y")
GBR25Y<-to.daily(GBR25Y)

AUS30Y<-data.frame(read.csv(file.path(Investing.com,"AUS30Y.csv")))
Investing.csv.xts(AUS30Y, type = "y")
AUS30Y<-to.daily(AUS30Y)

AUT30Y<-data.frame(read.csv(file.path(Investing.com,"AUT30Y.csv")))
Investing.csv.xts(AUT30Y, type = "y")
AUT30Y<-to.daily(AUT30Y)

CAN30Y<-data.frame(read.csv(file.path(Investing.com,"CAN30Y.csv")))
Investing.csv.xts(CAN30Y, type = "y")
CAN30Y<-to.daily(CAN30Y)

CHN30Y<-data.frame(read.csv(file.path(Investing.com,"CHN30Y.csv")))
Investing.csv.xts(CHN30Y, type = "y")
CHN30Y<-to.daily(CHN30Y)

DNK30Y<-data.frame(read.csv(file.path(Investing.com,"DNK30Y.csv")))
Investing.csv.xts(DNK30Y, type = "y")
DNK30Y<-to.daily(DNK30Y)

FIN30Y<-data.frame(read.csv(file.path(Investing.com,"FIN30Y.csv")))
Investing.csv.xts(FIN30Y, type = "y")
FIN30Y<-to.daily(FIN30Y)

FRA30Y<-data.frame(read.csv(file.path(Investing.com,"FRA30Y.csv")))
Investing.csv.xts(FRA30Y, type = "y")
FRA30Y<-to.daily(FRA30Y)

DEU30Y<-data.frame(read.csv(file.path(Investing.com,"DEU30Y.csv")))
Investing.csv.xts(DEU30Y, type = "y")
DEU30Y<-to.daily(DEU30Y)

IND30Y<-data.frame(read.csv(file.path(Investing.com,"IND30Y.csv")))
Investing.csv.xts(IND30Y, type = "y")
IND30Y<-to.daily(IND30Y)

IDN30Y<-data.frame(read.csv(file.path(Investing.com,"IDN30Y.csv")))
Investing.csv.xts(IDN30Y, type = "y")
IDN30Y<-to.daily(IDN30Y)

ISR30Y<-data.frame(read.csv(file.path(Investing.com,"ISR30Y.csv")))
Investing.csv.xts(ISR30Y, type = "y")
ISR30Y<-to.daily(ISR30Y)

ITA30Y<-data.frame(read.csv(file.path(Investing.com,"ITA30Y.csv")))
Investing.csv.xts(ITA30Y, type = "y")
ITA30Y<-to.daily(ITA30Y)

JPN30Y<-data.frame(read.csv(file.path(Investing.com,"JPN30Y.csv")))
Investing.csv.xts(JPN30Y, type = "y")
JPN30Y<-to.daily(JPN30Y)

MYS30Y<-data.frame(read.csv(file.path(Investing.com,"MYS30Y.csv")))
Investing.csv.xts(MYS30Y, type = "y")
MYS30Y<-to.daily(MYS30Y)

MEX30Y<-data.frame(read.csv(file.path(Investing.com,"MEX30Y.csv")))
Investing.csv.xts(MEX30Y, type = "y")
MEX30Y<-to.daily(MEX30Y)

NLD30Y<-data.frame(read.csv(file.path(Investing.com,"NLD30Y.csv")))
Investing.csv.xts(NLD30Y, type = "y")
NLD30Y<-to.daily(NLD30Y)

PER30Y<-data.frame(read.csv(file.path(Investing.com,"PER30Y.csv")))
Investing.csv.xts(PER30Y, type = "y")
PER30Y<-to.daily(PER30Y)

PRT30Y<-data.frame(read.csv(file.path(Investing.com,"PRT30Y.csv")))
Investing.csv.xts(PRT30Y, type = "y")
PRT30Y<-to.daily(PRT30Y)

QAT30Y<-data.frame(read.csv(file.path(Investing.com,"QAT30Y.csv")))
Investing.csv.xts(QAT30Y, type = "y")
QAT30Y<-to.daily(QAT30Y)

SGP30Y<-data.frame(read.csv(file.path(Investing.com,"SGP30Y.csv")))
Investing.csv.xts(SGP30Y, type = "y")
SGP30Y<-to.daily(SGP30Y)

ZAF30Y<-data.frame(read.csv(file.path(Investing.com,"ZAF30Y.csv")))
Investing.csv.xts(ZAF30Y, type = "y")
ZAF30Y<-to.daily(ZAF30Y)

KOR30Y<-data.frame(read.csv(file.path(Investing.com,"KOR30Y.csv")))
Investing.csv.xts(KOR30Y, type = "y")
KOR30Y<-to.daily(KOR30Y)

ESP30Y<-data.frame(read.csv(file.path(Investing.com,"ESP30Y.csv")))
Investing.csv.xts(ESP30Y, type = "y")
ESP30Y<-to.daily(ESP30Y)

CHE30Y<-data.frame(read.csv(file.path(Investing.com,"CHE30Y.csv")))
Investing.csv.xts(CHE30Y, type = "y")
CHE30Y<-to.daily(CHE30Y)

TWN30Y<-data.frame(read.csv(file.path(Investing.com,"TWN30Y.csv")))
Investing.csv.xts(TWN30Y, type = "y")
TWN30Y<-to.daily(TWN30Y)

GBR30Y<-data.frame(read.csv(file.path(Investing.com,"GBR30Y.csv")))
Investing.csv.xts(GBR30Y, type = "y")
GBR30Y<-to.daily(GBR30Y)

USA30Y<-data.frame(read.csv(file.path(Investing.com,"USA30Y.csv")))
Investing.csv.xts(USA30Y, type = "y")
USA30Y<-to.daily(USA30Y)

JPN40Y<-data.frame(read.csv(file.path(Investing.com,"JPN40Y.csv")))
Investing.csv.xts(JPN40Y, type = "y")
JPN40Y<-to.daily(JPN40Y)

GBR40Y<-data.frame(read.csv(file.path(Investing.com,"GBR40Y.csv")))
Investing.csv.xts(GBR40Y, type = "y")
GBR40Y<-to.daily(GBR40Y)

AUT50Y<-data.frame(read.csv(file.path(Investing.com,"AUT50Y.csv")))
Investing.csv.xts(AUT50Y, type = "y")
AUT50Y<-to.daily(AUT50Y)

CZE50Y<-data.frame(read.csv(file.path(Investing.com,"CZE50Y.csv")))
Investing.csv.xts(CZE50Y, type = "y")
CZE50Y<-to.daily(CZE50Y)

FRA50Y<-data.frame(read.csv(file.path(Investing.com,"FRA50Y.csv")))
Investing.csv.xts(FRA50Y, type = "y")
FRA50Y<-to.daily(FRA50Y)

ITA50Y<-data.frame(read.csv(file.path(Investing.com,"ITA50Y.csv")))
Investing.csv.xts(ITA50Y, type = "y")
ITA50Y<-to.daily(ITA50Y)

KOR50Y<-data.frame(read.csv(file.path(Investing.com,"KOR50Y.csv")))
Investing.csv.xts(KOR50Y, type = "y")
KOR50Y<-to.daily(KOR50Y)

CHE50Y<-data.frame(read.csv(file.path(Investing.com,"CHE50Y.csv")))
Investing.csv.xts(CHE50Y, type = "y")
CHE50Y<-to.daily(CHE50Y)

GBR50Y<-data.frame(read.csv(file.path(Investing.com,"GBR50Y.csv")))
Investing.csv.xts(GBR50Y, type = "y")
GBR50Y<-to.daily(GBR50Y)


#### Convert ETF data ................................................ ####
XLY<-data.frame(read.csv(file.path(Investing.com,"XLY.csv")))
Investing.csv.xts(XLY)
XLY<-to.daily(XLY)

XLP<-data.frame(read.csv(file.path(Investing.com,"XLP.csv")))
Investing.csv.xts(XLP)
XLP<-to.daily(XLP)

XLE<-data.frame(read.csv(file.path(Investing.com,"XLE.csv")))
Investing.csv.xts(XLE)
XLE<-to.daily(XLE)

XLF<-data.frame(read.csv(file.path(Investing.com,"XLF.csv")))
Investing.csv.xts(XLF)
XLF<-to.daily(XLF)

XLV<-data.frame(read.csv(file.path(Investing.com,"XLV.csv")))
Investing.csv.xts(XLV)
XLV<-to.daily(XLV)

XLI<-data.frame(read.csv(file.path(Investing.com,"XLI.csv")))
Investing.csv.xts(XLI)
XLI<-to.daily(XLI)

XLB<-data.frame(read.csv(file.path(Investing.com,"XLB.csv")))
Investing.csv.xts(XLB)
XLB<-to.daily(XLB)

XLRE<-data.frame(read.csv(file.path(Investing.com,"XLRE.csv")))
Investing.csv.xts(XLRE)
XLRE<-to.daily(XLRE)

XLK<-data.frame(read.csv(file.path(Investing.com,"XLK.csv")))
Investing.csv.xts(XLK)
XLK<-to.daily(XLK)

XLU<-data.frame(read.csv(file.path(Investing.com,"XLU.csv")))
Investing.csv.xts(XLU)
XLU<-to.daily(XLU)

XITK<-data.frame(read.csv(file.path(Investing.com,"XITK.csv")))
Investing.csv.xts(XITK)
XITK<-to.daily(XITK)

XNTK<-data.frame(read.csv(file.path(Investing.com,"XNTK.csv")))
Investing.csv.xts(XNTK)
XNTK<-to.daily(XNTK)

XAR<-data.frame(read.csv(file.path(Investing.com,"XAR.csv")))
Investing.csv.xts(XAR)
XAR<-to.daily(XAR)

KBE<-data.frame(read.csv(file.path(Investing.com,"KBE.csv")))
Investing.csv.xts(KBE)
KBE<-to.daily(KBE)

XBI<-data.frame(read.csv(file.path(Investing.com,"XBI.csv")))
Investing.csv.xts(XBI)
XBI<-to.daily(XBI)

KCE<-data.frame(read.csv(file.path(Investing.com,"KCE.csv")))
Investing.csv.xts(KCE)
KCE<-to.daily(KCE)

XHE<-data.frame(read.csv(file.path(Investing.com,"XHE.csv")))
Investing.csv.xts(XHE)
XHE<-to.daily(XHE)

XHS<-data.frame(read.csv(file.path(Investing.com,"XHS.csv")))
Investing.csv.xts(XHS)
XHS<-to.daily(XHS)

XHB<-data.frame(read.csv(file.path(Investing.com,"XHB.csv")))
Investing.csv.xts(XHB)
XHB<-to.daily(XHB)

KIE<-data.frame(read.csv(file.path(Investing.com,"KIE.csv")))
Investing.csv.xts(KIE)
KIE<-to.daily(KIE)

XWEB<-data.frame(read.csv(file.path(Investing.com,"XWEB.csv")))
Investing.csv.xts(XWEB)
XWEB<-to.daily(XWEB)

XME<-data.frame(read.csv(file.path(Investing.com,"XME.csv")))
Investing.csv.xts(XME)
XME<-to.daily(XME)

XES<-data.frame(read.csv(file.path(Investing.com,"XES.csv")))
Investing.csv.xts(XES)
XES<-to.daily(XES)

XOP<-data.frame(read.csv(file.path(Investing.com,"XOP.csv")))
Investing.csv.xts(XOP)
XOP<-to.daily(XOP)

XPH<-data.frame(read.csv(file.path(Investing.com,"XPH.csv")))
Investing.csv.xts(XPH)
XPH<-to.daily(XPH)

KRE<-data.frame(read.csv(file.path(Investing.com,"KRE.csv")))
Investing.csv.xts(KRE)
KRE<-to.daily(KRE)

XRT<-data.frame(read.csv(file.path(Investing.com,"XRT.csv")))
Investing.csv.xts(XRT)
XRT<-to.daily(XRT)

XSD<-data.frame(read.csv(file.path(Investing.com,"XSD.csv")))
Investing.csv.xts(XSD)
XSD<-to.daily(XSD)

XSW<-data.frame(read.csv(file.path(Investing.com,"XSW.csv")))
Investing.csv.xts(XSW)
XSW<-to.daily(XSW)

XTH<-data.frame(read.csv(file.path(Investing.com,"XTH.csv")))
Investing.csv.xts(XTH)
XTH<-to.daily(XTH)

XTL<-data.frame(read.csv(file.path(Investing.com,"XTL.csv")))
Investing.csv.xts(XTL)
XTL<-to.daily(XTL)

XTN<-data.frame(read.csv(file.path(Investing.com,"XTN.csv")))
Investing.csv.xts(XTN)
XTN<-to.daily(XTN)

RWO<-data.frame(read.csv(file.path(Investing.com,"RWO.csv")))
Investing.csv.xts(RWO)
RWO<-to.daily(RWO)

RWX<-data.frame(read.csv(file.path(Investing.com,"RWX.csv")))
Investing.csv.xts(RWX)
RWX<-to.daily(RWX)

RWR<-data.frame(read.csv(file.path(Investing.com,"RWR.csv")))
Investing.csv.xts(RWR)
RWR<-to.daily(RWR)

GLD<-data.frame(read.csv(file.path(Investing.com,"GLD.csv")))
Investing.csv.xts(GLD)
GLD<-to.daily(GLD)

SLV<-data.frame(read.csv(file.path(Investing.com,"SLV.csv")))
Investing.csv.xts(SLV)
SLV<-to.daily(SLV)

GLDW<-data.frame(read.csv(file.path(Investing.com,"GLDW.csv")))
Investing.csv.xts(GLDW)
GLDW<-to.daily(GLDW)

GII<-data.frame(read.csv(file.path(Investing.com,"GII.csv")))
Investing.csv.xts(GII)
GII<-to.daily(GII)

GNR<-data.frame(read.csv(file.path(Investing.com,"GNR.csv")))
Investing.csv.xts(GNR)
GNR<-to.daily(GNR)

NANR<-data.frame(read.csv(file.path(Investing.com,"NANR.csv")))
Investing.csv.xts(NANR)
NANR<-to.daily(NANR)