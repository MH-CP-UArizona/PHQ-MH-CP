* Encoding: UTF-8.

*open data file. 
*use data file with suffix 20241127.


******************************************************************************************************
*******************************************************************************************************
**********CREATE SYMPTOMS-RELATED VARIABLES***************************************
*******************************************************************************************************
******************************************************************************************************
 *MENTAL HEALTH SYMPTOMS.     
*create binary variables for mental health symptoms. 
RECODE GADCAT_A (SYSMIS=SYSMIS) (3=1)(4=1)(1=0)(2=0) INTO anxiety.
RECODE PHQCAT_A (SYSMIS=SYSMIS) (3=1)(4=1)(1=0)(2=0) INTO depression. 
   
*Create binary variables for PHQ symptoms.

RECODE PHQ81_A (SYSMIS=SYSMIS) (0=0)(1=1) (2=2) (3=3)  INTO anhedonia.
RECODE PHQ82_A (SYSMIS=SYSMIS) (0=0)(1=1) (2=2) (3=3)  INTO sadness.
RECODE PHQ83_A (SYSMIS=SYSMIS)  (0=0)(1=1) (2=2) (3=3) INTO sleep.
RECODE PHQ84_A (SYSMIS=SYSMIS) (0=0)(1=1) (2=2) (3=3) INTO energy.
RECODE PHQ85_A (SYSMIS=SYSMIS) (0=0)(1=1) (2=2) (3=3)  INTO  appetite.
RECODE PHQ86_A (SYSMIS=SYSMIS) (0=0)(1=1) (2=2) (3=3)INTO guilt.
RECODE PHQ87_A (SYSMIS=SYSMIS)  (0=0)(1=1) (2=2) (3=3) INTO concentration.
RECODE PHQ88_A (SYSMIS=SYSMIS)  (0=0)(1=1) (2=2) (3=3)INTO psychomotor.

*Create binary variables for GAD symptoms.
RECODE GAD71_A (SYSMIS=SYSMIS) (1=1)(2=2) (3=3)(4=4)  INTO nervous_on_edge.
RECODE GAD72_A (SYSMIS=SYSMIS)  (1=1)(2=2) (3=3)(4=4) INTO cant_stop_worry.
RECODE GAD73_A (SYSMIS=SYSMIS)  (1=1)(2=2) (3=3)(4=4)  INTO worry_differentThings.
RECODE GAD74_A (SYSMIS=SYSMIS)  (1=1)(2=2) (3=3)(4=4)  INTO relax_probs.
RECODE GAD75_A (SYSMIS=SYSMIS)  (1=1)(2=2) (3=3)(4=4)  INTO cant_sit_still.
RECODE GAD76_A (SYSMIS=SYSMIS)  (1=1)(2=2) (3=3)(4=4)  INTO too_irritable.
RECODE GAD77_A (SYSMIS=SYSMIS)  (1=1)(2=2) (3=3)(4=4)  INTO fear_future_dread.

COMPUTE PHQ8_count = SUM(anhedonia , sadness , sleep , energy , appetite ,  guilt , concentration , psychomotor).
EXECUTE.

    FREQUENCIES VARIABLES= PHQ8_count depression
  /ORDER=ANALYSIS.

    FREQUENCIES VARIABLES= anhedonia sadness sleep energy appetite guilt concentration psychomotor
  /ORDER=ANALYSIS.

    FREQUENCIES VARIABLES=nervous_on_edge cant_stop_worry worry_differentThings relax_probs cant_sit_still too_irritable fear_future_dread
  /ORDER=ANALYSIS.

RECODE anhedonia (SYSMIS=SYSMIS) (0=1)(1=0) (2=0) (3=0) INTO NOanhedonia.
RECODE sadness (SYSMIS=SYSMIS) (0=1)(1=0) (2=0) (3=0) INTO NOsadness.
RECODE sleep(SYSMIS=SYSMIS) (0=1)(1=0) (2=0) (3=0)INTO NOsleep_probs.
RECODE energy (SYSMIS=SYSMIS) (0=1)(1=0) (2=0) (3=0)INTO NOenergy_probs.
RECODE appetite (SYSMIS=SYSMIS) (0=1)(1=0) (2=0) (3=0) INTO NOappetite_probs.
RECODE guilt (SYSMIS=SYSMIS) (0=1)(1=0) (2=0) (3=0) INTO NOguilt.
RECODE concentration (SYSMIS=SYSMIS) (0=1)(1=0) (2=0) (3=0) INTO NOconcentration_probs.
RECODE psychomotor (SYSMIS=SYSMIS) (0=1)(1=0) (2=0) (3=0) INTO NOpsychomotor.

RECODE anhedonia (SYSMIS=SYSMIS) (0=0)(1=1) (2=0) (3=0) INTO SOMEanhedonia.
RECODE sadness (SYSMIS=SYSMIS) (0=0)(1=1) (2=0) (3=0) INTO SOMEsadness.
RECODE sleep (SYSMIS=SYSMIS) (0=0)(1=1) (2=0) (3=0) INTO SOMEsleep_probs.
RECODE energy (SYSMIS=SYSMIS) (0=0)(1=1) (2=0) (3=0) INTO SOMEenergy_probs.
RECODE appetite (SYSMIS=SYSMIS) (0=0)(1=1) (2=0) (3=0) INTO SOMEappetite_probs.
RECODE guilt (SYSMIS=SYSMIS) (0=0)(1=1) (2=0) (3=0) INTO SOMEguilt.
RECODE concentration(SYSMIS=SYSMIS) (0=0)(1=1) (2=0) (3=0) INTO SOMEconcentration_probs.
RECODE psychomotor (SYSMIS=SYSMIS) (0=0)(1=1) (2=0) (3=0) INTO SOMEpsychomotor_probs.

RECODE anhedonia (SYSMIS=SYSMIS) (0=0)(1=0) (2=1) (3=0) INTO MOSTLYanhedonia.
RECODE sadness (SYSMIS=SYSMIS) (0=0)(1=0) (2=1) (3=0) INTO MOSTLYsadness.
RECODE sleep (SYSMIS=SYSMIS) (0=0)(1=0) (2=1) (3=0)INTO MOSTLYsleep_probs.
RECODE energy (SYSMIS=SYSMIS) (0=0)(1=0) (2=1) (3=0)INTO MOSTLYenergy_probs.
RECODE appetite (SYSMIS=SYSMIS) (0=0)(1=0) (2=1) (3=0) INTO MOSTLYappetite_probs.
RECODE guilt (SYSMIS=SYSMIS) (0=0)(1=0) (2=1) (3=0) INTO MOSTLYguilt.
RECODE concentration (SYSMIS=SYSMIS) (0=0)(1=0) (2=1) (3=0) INTO MOSTLYconcentration_probs.
RECODE psychomotor (SYSMIS=SYSMIS) (0=0)(1=0) (2=1) (3=0) INTO MOSTLYpsychomotor_probs.


RECODE anhedonia (SYSMIS=SYSMIS) (0=0)(1=0) (2=0) (3=1)  INTO ALWAYSanhedonia.
RECODE sadness (SYSMIS=SYSMIS) (0=0)(1=0) (2=0) (3=1)  INTO ALWAYSsadness.
RECODE sleep (SYSMIS=SYSMIS) (0=0)(1=0) (2=0) (3=1)  INTO ALWAYSsleep_probs.
RECODE energy (SYSMIS=SYSMIS) (0=0)(1=0) (2=0) (3=1) INTO ALWAYSenergy_probs.
RECODE appetite (SYSMIS=SYSMIS) (0=0)(1=0) (2=0) (3=1) INTO ALWAYSappetite_probs.
RECODE guilt (SYSMIS=SYSMIS) (0=0)(1=0) (2=0) (3=1) INTO ALWAYSguilt.
RECODE concentration (SYSMIS=SYSMIS) (0=0)(1=0) (2=0) (3=1) INTO ALWAYSconcentration_probs.
RECODE psychomotor (SYSMIS=SYSMIS) (0=0)(1=0) (2=0) (3=1)  INTO ALWAYSpsychomotor_probs.

RECODE anhedonia (SYSMIS=SYSMIS) (0=0)(1=0)(2=1)(3=1) INTO ModSev_anhedonia.
RECODE sadness (SYSMIS=SYSMIS) (0=0)(1=0)(2=1)(3=1) INTO ModSev_sadness.
RECODE sleep (SYSMIS=SYSMIS) (0=0)(1=0)(2=1)(3=1)  INTO ModSev_sleep_probs.
RECODE energy (SYSMIS=SYSMIS) (0=0)(1=0)(2=1)(3=1)  INTO ModSev_energy_probs.
RECODE appetite (SYSMIS=SYSMIS) (0=0)(1=0)(2=1)(3=1)  INTO ModSev_appetite_probs.
RECODE guilt (SYSMIS=SYSMIS) (0=0)(1=0)(2=1)(3=1) INTO ModSev_guilt_probs.
RECODE concentration (SYSMIS=SYSMIS) (0=0)(1=0)(2=1)(3=1) INTO ModSev_concentration_probs.
RECODE psychomotor (SYSMIS=SYSMIS) (0=0)(1=0)(2=1)(3=1) INTO ModSev_psychomotor_probs.

**************************************GAD7*********************************************************.

RECODE nervous_on_edge (SYSMIS=SYSMIS) (1=1)(2=0)(3=0)(4=0) INTO NOnervous_on_edge.
RECODE cant_stop_worry (SYSMIS=SYSMIS) (1=1)(2=0)(3=0)(4=0) INTO NOcant_stop_worry.
RECODE worry_differentThings (SYSMIS=SYSMIS) (1=1)(2=0)(3=0)(4=0) INTO NOworry_differentThings.
RECODE relax_probs (SYSMIS=SYSMIS) (1=1)(2=0)(3=0)(4=0) INTO NOrelax_probs.
RECODE cant_sit_still (SYSMIS=SYSMIS) (1=1)(2=0)(3=0)(4=0) INTO NOcant_sit_still.
RECODE too_irritable (SYSMIS=SYSMIS) (1=1)(2=0)(3=0)(4=0) INTO NO_irritable.
RECODE fear_future_dread (SYSMIS=SYSMIS) (1=1)(2=0)(3=0)(4=0) INTO NOfear_future_dread.

RECODE nervous_on_edge (SYSMIS=SYSMIS) (1=0)(2=1)(3=0)(4=0)  INTO SOMEnervous_on_edge.
RECODE cant_stop_worry (SYSMIS=SYSMIS)  (1=0)(2=1)(3=0)(4=0)  INTO SOMEcant_stop_worry.
RECODE worry_differentThings (SYSMIS=SYSMIS)  (1=0)(2=1)(3=0)(4=0) INTO SOMEworry_differentThings.
RECODE relax_probs (SYSMIS=SYSMIS) (1=0)(2=1)(3=0)(4=0)  INTO SOMErelax_probs.
RECODE cant_sit_still (SYSMIS=SYSMIS)  (1=0)(2=1)(3=0)(4=0) INTO SOMEcant_sit_still.
RECODE too_irritable (SYSMIS=SYSMIS)  (1=0)(2=1)(3=0)(4=0) INTO SOME_irritable.
RECODE fear_future_dread (SYSMIS=SYSMIS)  (1=0)(2=1)(3=0)(4=0) INTO SOMEfear_future_dread.

RECODE nervous_on_edge (SYSMIS=SYSMIS)  (1=0)(2=0)(3=1)(4=0)  INTO MOSTLYnervous_on_edge.
RECODE cant_stop_worry (SYSMIS=SYSMIS)   (1=0)(2=0)(3=1)(4=0)  INTO MOSTLYcant_stop_worry.
RECODE worry_differentThings (SYSMIS=SYSMIS)  (1=0)(2=0)(3=1)(4=0)INTO MOSTLYworry_differentThings.
RECODE relax_probs (SYSMIS=SYSMIS)  (1=0)(2=0)(3=1)(4=0) INTO MOSTLYrelax_probs.
RECODE cant_sit_still (SYSMIS=SYSMIS) (1=0)(2=0)(3=1)(4=0) INTO MOSTLYcant_sit_still.
RECODE too_irritable (SYSMIS=SYSMIS)   (1=0)(2=0)(3=1)(4=0) INTO MOSTLY_irritable.
RECODE fear_future_dread (SYSMIS=SYSMIS)   (1=0)(2=0)(3=1)(4=0) INTO MOSTLYfear_future_dread.


RECODE nervous_on_edge (SYSMIS=SYSMIS)   (1=0)(2=0)(3=0)(4=1)  INTO ALWAYSnervous_on_edge.
RECODE cant_stop_worry (SYSMIS=SYSMIS)   (1=0)(2=0)(3=0)(4=1)  INTO ALWAYScant_stop_worry.
RECODE worry_differentThings (SYSMIS=SYSMIS)  (1=0)(2=0)(3=0)(4=1) INTO ALWAYSworry_differentThings.
RECODE relax_probs (SYSMIS=SYSMIS)   (1=0)(2=0)(3=0)(4=1)  INTO ALWAYSrelax_probs.
RECODE cant_sit_still (SYSMIS=SYSMIS) (1=0)(2=0)(3=0)(4=1)  INTO ALWAYScant_sit_still.
RECODE too_irritable (SYSMIS=SYSMIS)   (1=0)(2=0)(3=0)(4=1)  INTO ALWAYS_irritable.
RECODE fear_future_dread (SYSMIS=SYSMIS)    (1=0)(2=0)(3=0)(4=1) INTO ALWAYSfear_future_dread.


RECODE nervous_on_edge (SYSMIS=SYSMIS)   (1=0)(2=0)(3=1)(4=1) INTO ModSev_nervous_on_edge.
RECODE cant_stop_worry (SYSMIS=SYSMIS)   (1=0)(2=0)(3=1)(4=1)  INTO ModSev_cant_stop_worry.
RECODE worry_differentThings (SYSMIS=SYSMIS)  (1=0)(2=0)(3=1)(4=1) INTO ModSev_worry_differentThings.
RECODE relax_probs (SYSMIS=SYSMIS)    (1=0)(2=0)(3=1)(4=1) INTO ModSev_relax_probs.
RECODE cant_sit_still (SYSMIS=SYSMIS) (1=0)(2=0)(3=1)(4=1)  INTO ModSev_cant_sit_still.
RECODE too_irritable (SYSMIS=SYSMIS)   (1=0)(2=0)(3=1)(4=1) INTO ModSev_irritable.
RECODE fear_future_dread (SYSMIS=SYSMIS)    (1=0)(2=0)(3=1)(4=1) INTO ModSev_fear_future_dread.


COMPUTE mh_symptoms = $sysmis. 
execute. 
IF (depression = 1) OR (anxiety = 1) mh_symptoms = 1. 
IF(depression = 0) AND (anxiety = 0) mh_symptoms = 0.
EXECUTE.

COMPUTE mh_categorical = $sysmis. 
execute. 
IF (depression = 1) AND (anxiety = 0) mh_categorical = 1. 
IF (depression = 0) AND (anxiety = 1) mh_categorical = 2. 
IF (depression = 1) AND (anxiety = 1) mh_categorical = 3. 
IF (depression = 0) AND (anxiety = 0) mh_categorical = 4. 
EXECUTE.

Variable labels mh_categorical 'Type of mental health symptoms'.
    Value labels
     mh_categorical
     1 Depression Only
     2 Anxiety Only
     3 Both depression and anxiety
     4 Neither depression nor anxiety.
     Execute.

    FREQUENCIES VARIABLES= anxiety depression mh_symptoms mh_categorical
  /ORDER=ANALYSIS.

*CHRONIC PAIN SYMPTOMS.

RECODE PAIFRQ3M_A (SYSMIS=SYSMIS) (1=0) (2=0) (3=1) (4=1) INTO ChronicPain_any.
VARIABLE LABELS  ChronicPain_any 'Experiences Pain Most Days or Every Day'.
EXECUTE.

RECODE PAIFRQ3M_A (SYSMIS=SYSMIS) (1=0) (2=1) (3=1) (4=1) INTO pain_any.
VARIABLE LABELS  pain_any 'Experiences any pain'.
EXECUTE.

RECODE ChronicPain_any (SYSMIS=SYSMIS) (0=1)(1=0) INTO NO_ChronicPain_any.
VARIABLE LABELS  NO_ChronicPain_any 'DOES NOT Experience Pain Most Days or Every Day'.
execute.


*pain location/type.
RECODE PAIBACK3M_A (SYSMIS=SYSMIS)(1=0 )(2=0)(3=1)(4=1)  INTO ModSevBackPain.
RECODE PAIULMB3M_A (SYSMIS=SYSMIS) (1=0 )(2=0)(3=1)(4=1) INTO ModSevHandPain.
RECODE PAILLMB3M_A (SYSMIS=SYSMIS) (1=0 )(2=0)(3=1)(4=1) INTO ModSevHipPain.
RECODE PAIHDFC3M_A (SYSMIS=SYSMIS) (1=0 )(2=0)(3=1)(4=1) INTO ModSevMigraine.
RECODE PAIAPG3M_A (SYSMIS=SYSMIS) (1=0 )(2=0)(3=1)(4=1) INTO ModSevAbdominalPain.
RECODE PAITOOTH3M_A (SYSMIS=SYSMIS) (1=0 )(2=0)(3=1)(4=1) INTO ModSevToothPain.


RECODE PAIBACK3M_A (SYSMIS=SYSMIS)(1=0 )(2=1)(3=1)(4=1)  INTO anyBackPain.
RECODE PAIULMB3M_A (SYSMIS=SYSMIS) (1=0 )(2=1)(3=1)(4=1) INTO anyHandPain.
RECODE PAILLMB3M_A (SYSMIS=SYSMIS) (1=0 )(2=1)(3=1)(4=1) INTO anyHipPain.
RECODE PAIHDFC3M_A (SYSMIS=SYSMIS) (1=0 )(2=1)(3=1)(4=1) INTO anyMigraine.
RECODE PAIAPG3M_A (SYSMIS=SYSMIS) (1=0 )(2=1)(3=1)(4=1) INTO anyAbdominalPain.
RECODE PAITOOTH3M_A (SYSMIS=SYSMIS) (1=0 )(2=1)(3=1)(4=1) INTO anyToothPain.


RECODE PAIAMNT_A (SYSMIS=SYSMIS) (1=1)(2=3)(3=2) INTO pain_severity. 
VARIABLE LABELS pain_severity 'How much pain last time?'.
VALUE LABELS 
    pain_severity
    1 'A little '
    2 'Somewhere between a little and a lot'
    3 'A lot'.
    EXECUTE.


RECODE pain_severity (SYSMIS=SYSMIS)(1=1)(2=0)(3=0) INTO aLittlePain.
execute.

RECODE pain_severity (SYSMIS=SYSMIS) (1=0)(2=1)(3=0) INTO betweenLittleandLot.
execute.

RECODE pain_severity (SYSMIS=SYSMIS) (1=0)(2=0)(3=1) INTO aLotPain.
execute.

FREQUENCIES VARIABLES= PAIAMNT_A pain_severity aLittlePain betweenLittleandLot aLotPain.

FREQUENCIES VARIABLES= pain_severity.

**COMBINATION OF PAIN AND MENTAL HEALTH SYMPTOMS. 
***Create variable for combinations of mental heatlh and pain symtpsoms.
COMPUTE cooccurence = $sysmis.
EXECUTE.
IF (ChronicPain_any=0 and mh_symptoms=0) cooccurence = 1.
IF (ChronicPain_any=1 and mh_symptoms=0) cooccurence =2.
IF (ChronicPain_any=0 and mh_symptoms=1) cooccurence=3.
IF (ChronicPain_any=1 and mh_symptoms=1) cooccurence=4.
EXECUTE.
**Functional limitation variable among people who have chronic pain. 

COMPUTE HighImpactCP = $sysmis. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=3 or  PAIWKLM3M_A=4)  HighImpactCP = 1. 
IF  (ChronicPain_any=1) and (PAIWKLM3M_A=1 or  PAIWKLM3M_A=2)  HighImpactCP = 0.
EXECUTE.

COMPUTE LowerImpactCP = $sysmis. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=1 or  PAIWKLM3M_A=2)  LowerImpactCP = 1. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=3 or  PAIWKLM3M_A=4) LowerImpactCP = 0. 
EXECUTE.

FREQUENCIES VARIABLES= ChronicPain_any HighImpactCP LowerImpactCP .

COMPUTE HighImpactCP_largedenom = $sysmis. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=3 or  PAIWKLM3M_A=4)  HighImpactCP_largedenom = 1. 
IF  (ChronicPain_any=1) and (PAIWKLM3M_A=1 or  PAIWKLM3M_A=2)  HighImpactCP_largedenom = 0.
IF  (ChronicPain_any=0)  HighImpactCP_largedenom = 0.   
EXECUTE.

COMPUTE LowerImpactCP_largedenom = $sysmis. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=1 or  PAIWKLM3M_A=2)  LowerImpactCP_largedenom = 1. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=3 or  PAIWKLM3M_A=4) LowerImpactCP_largedenom = 0. 
IF  (ChronicPain_any=0)  LowerImpactCP_largedenom = 0.  
EXECUTE.

temporary. 
select if depression =1. 
FREQUENCIES VARIABLES= ChronicPain_any LowerImpactCP_largedenom HighImpactCP_largedenom.

***Impact on family, among people who have chronic pian. 

COMPUTE HighFAMImpactCP = $sysmis. 
IF (ChronicPain_any=1) and (PAIAFFM3M_A=3 or  PAIAFFM3M_A=4)  HighFAMImpactCP = 1. 
IF  (ChronicPain_any=1) and (PAIAFFM3M_A=1 or  PAIAFFM3M_A=2)  HighFAMImpactCP = 0.
EXECUTE.

COMPUTE LowerFAMImpactCP = $sysmis. 
IF (ChronicPain_any=1) and (PAIAFFM3M_A=1 or  PAIAFFM3M_A=2)  LowerFAMImpactCP = 1. 
IF (ChronicPain_any=1) and (PAIAFFM3M_A=3 or  PAIAFFM3M_A=4) LowerFAMImpactCP = 0. 
EXECUTE.

COMPUTE HighFAMImpactCP_largedenom = $sysmis. 
IF (ChronicPain_any=1) and (PAIAFFM3M_A=3 or  PAIAFFM3M_A=4)  HighFAMImpactCP_largedenom = 1. 
IF  (ChronicPain_any=1) and (PAIAFFM3M_A=1 or  PAIAFFM3M_A=2)  HighFAMImpactCP_largedenom = 0.
IF  (ChronicPain_any=0)  HighFAMImpactCP_largedenom = 0.   
EXECUTE.

COMPUTE HighFAMImpactCP_largedenomNESTED = $sysmis. 
IF (HighImpactCP_largedenom =1) and (PAIAFFM3M_A=3 or  PAIAFFM3M_A=4)  HighFAMImpactCP_largedenomNESTED = 1. 
IF  ((ChronicPain_any=1) OR (HighImpactCP_largedenom=1))  and (PAIAFFM3M_A=1 or  PAIAFFM3M_A=2)  HighFAMImpactCP_largedenomNESTED = 0.
IF  (ChronicPain_any=0)  HighFAMImpactCP_largedenomNESTED = 0.   
EXECUTE.

COMPUTE LowerFAMImpactCP_largedenom = $sysmis. 
IF (ChronicPain_any=1) and (PAIAFFM3M_A=1 or  PAIAFFM3M_A=2)  LowerFAMImpactCP_largedenom = 1. 
IF (ChronicPain_any=1) and (PAIAFFM3M_A=3 or  PAIAFFM3M_A=4) LowerFAMImpactCP_largedenom = 0. 
IF  (ChronicPain_any=0)  LowerFAMImpactCP_largedenom = 0.  
EXECUTE.

COMPUTE LowerFAMImpactCP_largedenomNESTED = $sysmis. 
IF (HighImpactCP_largedenom =1) and (PAIAFFM3M_A=3 or  PAIAFFM3M_A=4)  LowerFAMImpactCP_largedenomNESTED = 1. 
IF ((ChronicPain_any=1) OR (HighImpactCP_largedenom=1))  and (PAIAFFM3M_A=1 or  PAIAFFM3M_A=2)  LowerFAMImpactCP_largedenomNESTED = 0.
IF  (ChronicPain_any=0)  LowerFAMImpactCP_largedenomNESTED = 0.   
EXECUTE.

FREQUENCIES VARIABLES= HighFAMImpactCP HighFAMImpactCP_largedenom HighFAMImpactCP_largedenomNESTED.
EXECUTE. 
    
 FREQUENCIES VARIABLES=  LowerFAMImpactCP  LowerFAMImpactCP_largedenom LowerFAMImpactCP_largedenomNESTED. 
EXECUTE. 

FREQUENCIES VARIABLES=    ChronicPain_any  HighImpactCP_largedenom    HighFAMImpactCP_largedenomNESTED.
EXECUTE. 


COMPUTE ChronicPainOrdinal =$sysmis. 
IF (ChronicPain_any=0) ChronicPainOrdinal = 1.
IF (ChronicPain_any=1 and LowerImpactCP_largedenom=1) ChronicPainOrdinal =2.
IF (ChronicPain_any=1 and HighImpactCP_largedenom=1) ChronicPainOrdinal=3.
EXECUTE.
    


COMPUTE anyChronicMigraine = $sysmis. 
execute. 
IF (anyMigraine = 1) AND (ChronicPain_any = 1)  anyChronicMigraine = 1. 
IF(anyMigraine = 0) OR  (ChronicPain_any = 0) anyChronicMigraine = 0.
EXECUTE.

COMPUTE high_impact_ChronicMigraine = $sysmis. 
IF (anyChronicMigraine = 1) AND (HighImpactCP_largedenom = 1)  high_impact_ChronicMigraine = 1. 
IF(anyChronicMigraine = 0) OR  (HighImpactCP_largedenom = 0) high_impact_ChronicMigraine = 0.
EXECUTE.

COMPUTE high_FAMimpact_ChronicMigraine = $sysmis. 
execute. 
IF (high_impact_ChronicMigraine = 1) AND (HighFAMImpactCP_largedenom  = 1)  highFAMimpact_ChronicMigraine = 1. 
IF(high_impact_ChronicMigraine = 0) OR  (HighFAMImpactCP_largedenom = 0) highFAMimpact_ChronicMigraine = 0.
EXECUTE.


VARIABLE LABELS cooccurence 'Presence of Symptoms: Chronic Pain, Mental Health, or Both'.
VALUE LABELS 
    cooccurence
    1 'No chronic pain or MH symptoms'
    2 'Chronic Pain (Only)'
    3 'Mental Health (Only)'
    4 'Co-Occuring Symptoms'.
    EXECUTE.

    FREQUENCIES VARIABLES= cooccurence
  /ORDER=ANALYSIS.

temporary. 
Select if cooccurence ne 1. 
    FREQUENCIES VARIABLES= cooccurence
  /ORDER=ANALYSIS.


***Create chronic migraine cooocurerence variable. 
COMPUTE chronic_migraine_cooccurence = $sysmis.
EXECUTE.
IF (anyChronicMigraine=0 and mh_symptoms=0) chronic_migraine_cooccurence = 1.
IF (anyChronicMigraine=1 and mh_symptoms=0) chronic_migraine_cooccurence =2.
IF (anyChronicMigraine=0 and mh_symptoms=1) chronic_migraine_cooccurence=3.
IF (anyChronicMigraine=1 and mh_symptoms=1) chronic_migraine_cooccurence=4.
EXECUTE.

***Create chronic migraine coocurrence variable. 
VARIABLE LABELS chronic_migraine_cooccurence 'Presence of Symptoms: Chronic Pain, Mental Health, or Both'.
VALUE LABELS 
    chronic_migraine_cooccurence
    1 'No chronic migraine or MH symptoms'
    2 'Chronic migraine (Only)'
    3 'Mental Health (Only)'
    4 'Co-Occuring Symptoms'.
    EXECUTE.

    FREQUENCIES VARIABLES= chronic_migraine_cooccurence
  /ORDER=ANALYSIS.
temporary. 
Select if chronic_migraine_cooccurence ne 1. 
    FREQUENCIES VARIABLES= chronic_migraine_cooccurence
  /ORDER=ANALYSIS.


******************************************************************************************************
*******************************************************************************************************
**********CREATE SCREENING AND REFERRAL-RELATED VARIABLES***************************************
*******************************************************************************************************
******************************************************************************************************
*create variable for doctor ever told you you have anxiety? doctor ever told you you have depression. 

RECODE DEPEV_A (SYSMIS=SYSMIS) (1=1) (2=0) INTO DocSaidDepress.
VARIABLE LABELS  DocSaidDepress 'Has a doctor or other heatlh professional ever told you that you have depression? '.
VALUE LABELS 
    DocSaidDepress
     1 'I have been told I have depression' 
     0 'I have NOT EVER been told I have depression'.

RECODE ANXEV_A (SYSMIS=SYSMIS) (1=1) (2=0) INTO DocSaidAnxiety.
VARIABLE LABELS  DocSaidAnxiety 'Has a doctor or other heatlh professional ever told you that you have anxiety? '.
VALUE LABELS 
    DocSaidAnxiety
     1 'I have been told I have anxiety' 
     0 'I have NOT EVER been told I have anxiety'.

COMPUTE DocSaidAnyMH = $sysmis. 
IF (DocSaidAnxiety=1) OR   (DocSaidDepress = 1) DocSaidAnyMH=1.
IF (DocSaidAnxiety=0) AND  (DocSaidDepress = 0) DocSaidAnyMH=0.
VARIABLE LABELS  DocSaidAnyMH 'Health Professional has said MH '.
VALUE LABELS 
    DocSaidAnyMH
     1 'I have been told I have anxiety or depression' 
     0 'I have NOT EVER been told I have anxiety or depression'.
EXECUTE.

RECODE DocSaidAnyMH (SYSMIS=SYSMIS) (0=1)(1=0) INTO DocNEVERsaidMH.
execute.
VARIABLE LABELS  DocNEVERsaidMH 'Health Professional has NOT EVER said MH'.
VALUE LABELS 
    DocNEVERsaidMH
     1 'I have NOT EVER been told I have anxiety or depression' 
     0 'I have been told I have anxiety or depression'.
EXECUTE.

FREQUENCIES VARIABLES= ANXEV_A  DocSaidAnxiety 
  /ORDER=ANALYSIS.


FREQUENCIES VARIABLES= DEPEV_A  DocSaidDepress 
  /ORDER=ANALYSIS.

FREQUENCIES VARIABLES= DEPEV_A  DocSaidDepress DocSaidAnyMH DocNEVERsaidMH 
  /ORDER=ANALYSIS.

RECODE USUALPL_A (SYSMIS=SYSMIS) (1=0)(2=1) INTO NOusualplace.
execute.
FREQUENCIES VARIABLES= NOusualplace USUALPL_A
  /ORDER=ANALYSIS.


RECODE EMPPDSKLV_A (SYSMIS=SYSMIS) (1=0)(2=1) INTO NOpaidSickLeave.
execute.
FREQUENCIES VARIABLES= EMPPDSKLV_A NOpaidSickLeave
  /ORDER=ANALYSIS.

RECODE EMPWRKLSWK_A (SYSMIS=SYSMIS) (1=0)(2=1) INTO NOworkedLastWeek.
execute.
FREQUENCIES VARIABLES= EMPWRKLSWK_A NOworkedLastWeek
  /ORDER=ANALYSIS.

******************************************************************************************************
*******************************************************************************************************
**********CREATE TREATMENT-RELATED VARIABLES***************************************
*******************************************************************************************************
******************************************************************************************************
NOTES ON RELEVANT TREATMENT VARIABLES.

*MHTHRPY_A  (asked of all respondents) 
Question Text: During the past 12 months, did you receive counseling or therapy from a mental
health professional such as a psychiatrist, psychologist, psychiatric nurse,
or clinical social worker?
Description: Received counseling/therapy from mental health professional, past 12m .

                       * MHTPYNOW_A      (Note, only a SUBSET OF MHTHRPY_A was asked this)
                        Are you currently receiving counseling or therapy from a mental health
                        professional?
                        Description: Currently receiving counseling/therapy from mental health professional
                        Recode:
                        Universe: HHSTAT_A=1 and MHTHRPY_A IN (1,7,9)
                        Universe Description: Sample adults 18+ who have received, or refused or don't know if they have
                        received counseling or therapy from a mental heatlh professional in last 12 months. 


*MHTHND_A (asked of all respondents)
Question Text: During the past 12 months, was there any time when you needed counseling or
therapy from a mental health professional, but DID NOT GET IT because of the cost?
Description: Needed counseling/therapy but did not get it due to cost, past 12m

 *DEPMED_A (asked of all respondents) 
Question Text: Do you take prescription medication for depression?
Description: Take medication for depression.  

*ANXMED_A (asked of all respondents) 
Question Text: Do you take prescription medication for these feelings?
Description: Take medication for worried/nervous/anxious feelings.
                       
                            
                            *MHRX_A (Subset of respondents, those who did not say they CURRENTLY  take anxiety or depression medication) 
                            Question Text: During the past 12 months, did you take prescription medication to help you
                            with any other emotions or with your concentration, behavior or mental health?
                            Description: Took medicine for other emotions/concentration/behavior/mental health, past 12m
                            Universe: HHSTAT_A=1 and ANXMED_A IN (2,7,9) and DEPMED_A IN (2,7,9)
                            Universe Description: Sample adults 18+ who have not taken medication for worry or don't know or
                                                                    refused if they have AND have not taken medication for depression or don't
                                                                    know or refused if they have. 

*CREATE MENTAL HEALTH TREATMENT VARIABLES. 
*create variable any_mh_med_12m measuring taken any medication for mental health in past 12m. 

RECODE ANXMED_A (SYSMIS=SYSMIS) (1=1) (2=0)  INTO anxiety_med_NOW.
RECODE DEPMED_A (SYSMIS=SYSMIS) (1=1) (2=0)  INTO depress_med_NOW.
RECODE MHRX_A (SYSMIS=SYSMIS) (1=1) (2=0)  INTO other_mh_med_12m.

COMPUTE any_mh_med_12m = $sysmis.
If (anxiety_med_NOW = 1) or (depress_med_NOW = 1) or  (other_mh_med_12m=1) any_mh_med_12m =1.
IF (anxiety_med_NOW = 0) AND (depress_med_NOW = 0) AND (other_mh_med_12m=0) any_mh_med_12m =0.
EXECUTE.


COMPUTE any_ad_med_12m = $sysmis.
If (anxiety_med_NOW = 1) or (depress_med_NOW = 1)  any_ad_med_12m =1.
IF (anxiety_med_NOW = 0) AND (depress_med_NOW = 0)  any_ad_med_12m =0.
EXECUTE.

FREQUENCIES VARIABLES= any_mh_med_12m
  /ORDER=ANALYSIS.
*create variable any_mh_therapy12M measuring whether any therapy for mental health was used in past 12m. 

RECODE MHTHRPY_A (SYSMIS=SYSMIS) (1=1) (2=0)  INTO any_mh_therapy_12m.

FREQUENCIES VARIABLES= any_mh_therapy_12m
  /ORDER=ANALYSIS.

*create variable MH_therapy_NOW, ensure that zeros for MH therapy 12m are listed as zeros for MH therapy now, and not as missings--an artifact created by subsetting .
RECODE MHTPYNOW_A (SYSMIS=SYSMIS) (1=1) (2=0)  INTO MH_therapy_NOW.
IF (any_mh_therapy_12m = 0) MH_therapy_NOW =0.
EXECUTE.

FREQUENCIES VARIABLES= any_mh_therapy_12m MH_therapy_NOW 
  /ORDER=ANALYSIS.

*create variable that measures existence of any mental health treatment - past 12m. 

COMPUTE any_mh_tx_12m = $sysmis.
If (any_mh_med_12m = 1)  OR  (any_mh_therapy_12m=1) any_mh_tx_12m =1.
If (any_mh_med_12m = 0)  AND  (any_mh_therapy_12m=0) any_mh_tx_12m =0.
EXECUTE.

COMPUTE any_mh_tx_NOW = $sysmis.
EXECUTE.
If (Anxiety_Med_NOW = 1) or (Depress_Med_NOW = 1) or  (MH_therapy_NOW=1) any_mh_tx_NOW =1.
IF ((Anxiety_Med_NOW = 0) AND (Depress_Med_NOW  = 0) AND (MH_therapy_NOW =0)) any_mh_tx_NOW =0.
EXECUTE.


FREQUENCIES VARIABLES= any_mh_tx_12m any_mh_tx_NOW 
  /ORDER=ANALYSIS.

*create variable that measures 12m mental health treatment approach: meds only, counseling only, both, neither. 
COMPUTE mh_tx_approach_12m = $sysmis.
EXECUTE.
If (any_mh_med_12m = 1)  AND (any_mh_therapy_12m=0) mh_tx_approach_12m =1.
IF (any_mh_med_12m = 0)  AND (any_mh_therapy_12m =1) mh_tx_approach_12m =2.
IF (any_mh_med_12m = 1)  AND (any_mh_therapy_12m =1) mh_tx_approach_12m =3.
IF (any_mh_med_12m = 0)  AND (any_mh_therapy_12m =0) mh_tx_approach_12m =4.
EXECUTE.

VARIABLE LABELS mh_tx_approach_12m 'MH Treatment Approach - past 12m'.
Value Labels 
    mh_tx_approach_12m
    1 'Medication only'
    2 'Therapy only'
    3 'Both medication and therapy'
    4 'No A/D treatment'.

FREQUENCIES VARIABLES= mh_tx_approach_12m
  /ORDER=ANALYSIS.

*PAIN TREATMENT. 
**Recode pain management strategies ino binary strategies.

RECODE PAIPHYSTPY_A (SYSMIS=SYSMIS) (1=1) (2=0) into pt.
RECODE PAICHIRO_A (SYSMIS=SYSMIS) (1=1) (2=0) into chiro.
RECODE PAITALKTPY_A (SYSMIS=SYSMIS) (1=1) (2=0) into cbt_pain.
RECODE PAIPROGRAM_A (SYSMIS=SYSMIS) (1=1) (2=0) into self_mgmt.
RECODE PAIGROUP_A (SYSMIS=SYSMIS) (1=1) (2=0) into peer_sppt.
RECODE PAIYOGA_A (SYSMIS=SYSMIS) (1=1) (2=0) into yoga.
RECODE PAIMASSAGE_A (SYSMIS=SYSMIS) (1=1) (2=0) into massage.
RECODE PAIMEDITAT_A (SYSMIS=SYSMIS) (1=1) (2=0) into meditation.
RECODE PAIMOTHER_A (SYSMIS=SYSMIS) (1=1) (2=0) into other_pain_tx.

FREQUENCIES VARIABLES= pt chiro cbt_pain self_mgmt peer_sppt massage meditation other_pain_tx   
  /ORDER=ANALYSIS.

*opioids.
RECODE RX12M_A (SYSMIS=SYSMIS) (1=1)(2=0) INTO Any_Rx12m.
RECODE OPD12M_A (SYSMIS=SYSMIS) (1=1)(2=0) INTO AnyOpioid12m.
RECODE OPD3M_A (SYSMIS=SYSMIS) (1=1)(2=0) INTO AnyOpioid3m.
RECODE OPDACUTE_A (SYSMIS=SYSMIS) (1=1)(2=0) INTO AcuteOpioid3m.
RECODE OPDCHRONIC_A (SYSMIS=SYSMIS) (1=1)(2=0) INTO ChronicOpioid3m.
RECODE OPDFREQ_A (SYSMIS=SYSMIS) (1=1)(2=2)(3=3) INTO ChronicOpdFreq3m.
EXECUTE. 


FREQUENCIES VARIABLES= Any_Rx12m AnyOpioid12m AnyOpioid3m AcuteOpioid3m ChronicOpioid3m ChronicOpdFreq3m
  /ORDER=ANALYSIS.

COMPUTE AnyOpioid12m_NEW = $sysmis. 
IF (Any_Rx12m = 7) AnyOpioid12m_NEW = 7.  
IF (Any_Rx12m = 1) and (AnyOpioid12m = 0) AnyOpioid12m_NEW=0.
IF  (Any_Rx12m = 1) and (AnyOpioid12m = 1)  AnyOpioid12m_NEW = 1.
EXECUTE. 


COMPUTE AnyOpioid3m_NEW = $sysmis. 
IF (Any_Rx12m = 0) AnyOpioid3m_NEW = 0.  
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 0) AnyOpioid3m_NEW = 0  .  
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 1) and (AnyOpioid3m = 0) AnyOpioid3m_NEW = 0. 
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 1) and (AnyOpioid3m = 1) AnyOpioid3m_NEW = 1. 
EXECUTE. 

COMPUTE AcuteOpioid3m_NEW = $sysmis. 
IF (Any_Rx12m = 0) AcuteOpioid3m_NEW = 0.  
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 0) AcuteOpioid3m_NEW = 0  .  
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 1) and (AnyOpioid3m = 0) AcuteOpioid3m_NEW = 0. 
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 1) and (AnyOpioid3m = 1) AND (AcuteOpioid3m = 0)  AcuteOpioid3m_NEW = 0. 
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 1) and (AnyOpioid3m = 1) AND (AcuteOpioid3m = 1)  AcuteOpioid3m_NEW = 1. 
EXECUTE. 

COMPUTE ChronicOpioid3m_NEW = $sysmis.  
IF (Any_Rx12m = 0) ChronicOpioid3m_NEW = 0.  
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 0) ChronicOpioid3m_NEW = 0  .  
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 1) and (AnyOpioid3m = 0) ChronicOpioid3m_NEW = 0. 
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 1) and (AnyOpioid3m = 1) AND (ChronicOpioid3m = 0)  ChronicOpioid3m_NEW = 0. 
IF (Any_Rx12m = 1)  AND (AnyOpioid12m = 1) and (AnyOpioid3m = 1) AND (ChronicOpioid3m = 1)  ChronicOpioid3m_NEW = 1. 
EXECUTE. 

FREQUENCIES VARIABLES= Any_Rx12m   AnyOpioid12m  AnyOpioid12m_NEW  AnyOpioid3m  AnyOpioid3m_NEW AcuteOpioid3m  AcuteOpioid3m_NEW ChronicOpioid3m  ChronicOpioid3m_NEW 
  /ORDER=ANALYSIS.

*pain treament appraoch.
   * COMPUTE pain_tx_approach = $sysmis.
*EXECUTE.
*If ((pt = 1) or (chiro = 1) or  ( =1)) and (MH_therapy=0) pain_tx_approach =1.
*IF (Anxiety_Med = 0) AND (Depress_Med = 0) AND  (Other_MH_Med = 0) AND (MH_therapy =1) pain_tx_approach =2.
*IF ((Anxiety_Med = 1) OR (Depress_Med = 1) OR  (Other_MH_Med = 1)) AND (MH_therapy =1) pain_tx_approach =3.
*IF ((Anxiety_Med = 0) AND (Depress_Med = 0) AND  (Other_MH_Med = 0)) AND (MH_therapy =0) pain_tx_approach =4.
*EXECUTE.FREQUENCIES VARIABLES= pt chiro cbt_pain self_mgmt peer_sppt massage meditation other_pain_tx
 * /ORDER=ANALYSIS.

*********FIGURE THIS ONE OUT************************************.
*VARIABLE LABELS pain_tx_approach 'MH Treatment Approach'.
*Value Labels
    *pain_tx_approach
    *1 "Opioids only"
    *2 ''Provider required"  
   * 3 'Self Or Peer Mgmt'
   * 4 'No MH treatment'.
*Execute. 

*make variable for any pain treatment. * FIX THIS ONE.
COMPUTE any_pain_treatment = $sysmis. 
 IF ( (ChronicOpioid3m_NEW=1) or (pt =1) OR (chiro =1) OR (cbt_pain =1) OR (self_mgmt=1)  OR (peer_sppt=1)  OR (yoga=1) OR (massage=1) OR (meditation= 1) OR (other_pain_tx= 1) ) any_pain_treatment = 1. 
IF ((ChronicOpioid3m_NEW=0) AND (pt =0) AND (chiro =0) AND (cbt_pain =0) AND (self_mgmt=0)  AND (peer_sppt=0)  AND (yoga=0) AND (massage=0) AND (meditation= 0) AND (other_pain_tx= 0) ) any_pain_treatment = 0. 
EXECUTE.

RECODE any_pain_treatment (SYSMIS=SYSMIS)(1=0)(0=1) into NO_pain_treatment. 
EXECUTE.

FREQUENCIES VARIABLES=NO_pain_treatment any_pain_treatment
  /ORDER=ANALYSIS.



*Make variable for integrated pain and MH approach. 
COMPUTE Integrated_tx_approach = $sysmis  .
execute.  
 IF (any_pain_treatment=0) AND (any_mh_tx_NOW=0) Integrated_tx_approach = 1. 
 IF (any_pain_treatment=1) AND (any_mh_tx_NOW=0) Integrated_tx_approach = 2. 
 IF (any_pain_treatment=0) AND (any_mh_tx_NOW=1) Integrated_tx_approach = 3. 
 IF (any_pain_treatment=1) AND (any_mh_tx_NOW=1) Integrated_tx_approach = 4.  
EXECUTE.

VARIABLE LABELS Integrated_tx_approach 'Use of Pain and_or Mental Health Treatment'.
VALUE LABELS 
    Integrated_tx_approach
    1 'No pain treatment and no MH treatment'
    2 'Pain treatment only '
    3 'MH treatment only'
    4 'Both Pain and MH treatment'.
Execute. 
FREQUENCIES VARIABLES= any_pain_treatment Integrated_tx_approach any_mh_tx_NOW
  /ORDER=ANALYSIS.


******************************************************************************************************
*******************************************************************************************************
**********CREATE COMBINATION OF TREATMENT AND NEED" VARIABLES***************************************
*******************************************************************************************************
******************************************************************************************************

**Create binaries for combinations of treatment and need.
COMPUTE txANDsymptoms =  $sysmis. 
EXECUTE.
if (any_mh_tx_12m = 1) and (mh_symptoms=1)  txANDsymptoms = 1.
if (any_mh_tx_12m  = 0) OR (mh_symptoms = 0) txANDsymptoms =0.
EXECUTE.

COMPUTE symptomsNOtx =  $sysmis. 
EXECUTE.
if (mh_symptoms=1) and  (any_mh_tx_12m = 0) symptomsNOtx =1.
if (mh_symptoms =0) OR (any_mh_tx_12m  = 1)  symptomsNOtx = 0.
EXECUTE.

COMPUTE NoSymptomsNOtx =  $sysmis. 
EXECUTE.
if (mh_symptoms =0) AND (any_mh_tx_12m  = 0) NoSymptomsNOtx =1.
if (mh_symptoms = 1) OR (any_mh_tx_12m =1)  NoSymptomsNOtx = 0.
EXECUTE.

COMPUTE txNOsymptoms =  $sysmis. 
EXECUTE. 
if (any_mh_tx_12m =1) and (mh_symptoms = 0) txNOsymptoms =1.
if (any_mh_tx_12m  = 0) OR (mh_symptoms=1)  txNOsymptoms = 0.
EXECUTE.


COMPUTE mh_utiliz_need  = $sysmis. 
execute. 
IF (NoSymptomsNOtx=1) mh_utiliz_need = 1. 
IF (txNOsymptoms=1)  mh_utiliz_need = 2. 
 IF (txANDsymptoms=1)  mh_utiliz_need = 3.    
 IF (symptomsNOtx=1)  mh_utiliz_need = 4.    
EXECUTE.

VARIABLE LABELS mh_utiliz_need 'MH Treament Utilization and Presence of Symptoms'.
VALUE LABELS 
    mh_utiliz_need
    1 'No MH symptoms and no MH treatment'
    2 'MH symptoms controlled with MH treatment'
    3 'MH symptoms poorly controlled despite MH Treatment'
    4 'MH symptoms and not using MH Treatments'.
Execute. 

FREQUENCIES VARIABLES= txANDsymptoms symptomsNOtx NoSymptomsNOtx txNOsymptoms mh_utiliz_need
  /ORDER=ANALYSIS.

COMPUTE MHrelevance = $sysmis. 
EXECUTE. 
IF (NoSymptomsNOtx = 0) MHrelevance = 1. 
IF (NoSymptomsNOtx =1) MHrelevance = 0. 
EXECUTE. 

FREQUENCIES VARIABLES= NoSymptomsNOtx MHrelevance
  /ORDER=ANALYSIS.

COMPUTE MHscreeningAccurate = $sysmis. 
execute. 
IF (MHrelevance=1) and (DocSaidAnyMH=1)  MHscreeningAccurate = 1. 
IF (MHrelevance=0) and (DocSaidAnyMH=1)  MHscreeningAccurate = 2. 
IF (MHrelevance=0) and (DocSaidAnyMH=0)  MHscreeningAccurate = 3. 
IF (MHrelevance=1) and (DocSaidAnyMH=0)  MHscreeningAccurate = 4. 
EXECUTE.

Variable labels MHscreeningAccurate 'Screening Accuracy'.
  Value labels
    MHscreeningAccurate
    1 Correctly Advised
    2 Incorrectly Advised
    3 Correctly Did Not Advise
    4 Incorrectly Did Not Advise.
Execute.


******************************************************************************************************
*******************************************************************************************************
**GENERAL HEALTH STATUS AND FUNCTIONAL OUTCOMES**************
******************************************************************************************************
*******************************************************************************************************
*recode variable for general health status. 
RECODE PHSTAT_A (SYSMIS=SYSMIS) (1=5)(2=4)(3=3)(4=2)(5=1) INTO healthstatus.
Variable Labels healthstatus 'General Health Status'.
Value Labels
    healthstatus
    1 'Poor'
    2 'Fair'
    3 'Good'
    4 'Very Good'
    5 'Excellent'.
    EXECUTE.

FREQUENCIES VARIABLES= healthstatus
  /ORDER=ANALYSIS.

*Functional limitaions. 
RECODE SOCWRKLIM_A (SYSMIS=SYSMIS) (1=1) (2=0) into soc_work_limit.
RECODE SOCERRNDS_A (SYSMIS=SYSMIS) (1=0) (2=1)(3=1)(4=1) into soc_errands_limit.
RECODE SOCSCLPAR_A (SYSMIS=SYSMIS) (1=0) (2=1)(3=1)(4=1) into soc_participate_limit.

*create variable for functional limitations. 

COMPUTE any_functional_limit = $sysmis. 
execute. 
IF (soc_work_limit=1) or  (soc_errands_limit=1) or (soc_participate_limit=1)  any_functional_limit = 1. 
IF (soc_work_limit=0) and  (soc_errands_limit=0) and (soc_participate_limit=0) any_functional_limit =0. 
EXECUTE.

Variable Labels any_functional_limit 'Is functional Limitation Present'.
Value Labels
    any_functional_limit
    1 'Yes'
    0 'No'.
    EXECUTE.

*Create variables for types of functional limitations.
COMPUTE func_Limitations_combos = $sysmis.
IF (soc_work_limit=0) and  (soc_errands_limit=0) and (soc_participate_limit=0)  func_Limitations_combos = 0. 

IF (soc_work_limit=1) and  (soc_errands_limit=0) and (soc_participate_limit=0)  func_Limitations_combos = 1. 
IF (soc_work_limit=0) and  (soc_errands_limit=1) and (soc_participate_limit=0)  func_Limitations_combos = 2. 
IF (soc_work_limit=0) and  (soc_errands_limit=0) and (soc_participate_limit=1)  func_Limitations_combos = 3. 

IF (soc_work_limit=1) and  (soc_errands_limit=1) and (soc_participate_limit=0)  func_Limitations_combos = 2. 
IF (soc_work_limit=1) and  (soc_errands_limit=0) and (soc_participate_limit=1)  func_Limitations_combos = 2. 
IF (soc_work_limit=0) and  (soc_errands_limit=1) and (soc_participate_limit=1)  func_Limitations_combos = 2.
 
IF (soc_work_limit=1) and  (soc_errands_limit=1) and (soc_participate_limit=0)  func_Limitations_combos = 2. 
IF (soc_work_limit=1) and  (soc_errands_limit=1) and (soc_participate_limit=0)  func_Limitations_combos = 2. 
Execute. 


*compute binaries for combinations of treatment and functional limitation. 
COMPUTE txANDlimitation =  $sysmis. 
EXECUTE.
if (any_mh_tx_12m = 1) and (any_functional_limit=1)  txANDlimitation = 1.
if (any_mh_tx_12m = 0) OR (any_functional_limit = 0) txANDlimitation =0.
EXECUTE.

COMPUTE limitationNOtx =  $sysmis. 
EXECUTE.
if (any_functional_limit=1) and  (any_mh_tx_12m = 0) limitationNOtx =1.
if (any_functional_limit =0) OR (any_mh_tx_12m = 1)  limitationNOtx = 0.
EXECUTE.

COMPUTE NoLimitationNOtx =  $sysmis. 
EXECUTE.
if (any_functional_limit =0) AND (any_mh_tx_12m = 0) NoLimitationNOtx =1.
if (any_functional_limit = 1) OR (any_mh_tx_12m=1)  NoLimitationNOtx = 0.
EXECUTE.

COMPUTE txNOlimitation =  $sysmis. 
EXECUTE. 
if (any_mh_tx_12m=1) and (any_functional_limit = 0) txNOlimitation =1.
if (any_mh_tx_12m = 0) OR (any_functional_limit=1)  txNOlimitation = 0.
EXECUTE.


COMPUTE mh_utiliz_function  = $sysmis. 
execute. 
IF (NoLimitationNOtx=1) mh_utiliz_function = 1. 
IF (txNOlimitation=1)  mh_utiliz_function = 2. 
 IF (txANDlimitation=1)  mh_utiliz_function = 3.    
 IF (limitationNOtx=1)  mh_utiliz_function = 4.    
EXECUTE.

VARIABLE LABELS mh_utiliz_function 'MH Treament Utilization and Presence of Functional Limitation'.
VALUE LABELS 
    mh_utiliz_function
    1 'No functional limitation and no MH treatment'
    2 'MH treatment and no functional limitation'
    3 'MH Treatment and functional limitation'
    4 'Functional limitation and not using MH Treatments'.
Execute. 

FREQUENCIES VARIABLES= NoLimitationNOtx txNOlimitation txANDlimitation limitationNOtx  txNOsymptoms mh_utiliz_function
  /ORDER=ANALYSIS.

*pain efficacy. 
   RECODE PAINMEFF_A (SYSMIS=SYSMIS) (1=1) (2=0) into effective. 




*Functional impact of pain binaries. 
COMPUTE CPNeverLimits = $sysmis. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=1)  CPNeverLimits = 1. 
IF (ChronicPain_any=1) and ( PAIWKLM3M_A=2 Or PAIWKLM3M_A=3 or  PAIWKLM3M_A=4)  CPNeverLimits = 0. 
EXECUTE.

COMPUTE CPSometimesLimits = $sysmis. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=2)  CPSometimesLimits = 1. 
IF (ChronicPain_any=1) and ( PAIWKLM3M_A=1 Or PAIWKLM3M_A=3 or  PAIWKLM3M_A=4)  CPSometimesLimits = 0. 
EXECUTE.

COMPUTE CPMostDaysLimits = $sysmis. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=3)  CPMostDaysLimits = 1. 
IF (ChronicPain_any=1) and ( PAIWKLM3M_A=1 Or PAIWKLM3M_A=2 or  PAIWKLM3M_A=4)  CPMostDaysLimits = 0. 
EXECUTE.

COMPUTE CPEveryDayLimits = $sysmis. 
IF (ChronicPain_any=1) and (PAIWKLM3M_A=4)  CPEveryDayLimits = 1. 
IF (ChronicPain_any=1) and ( PAIWKLM3M_A=1 Or PAIWKLM3M_A=2 or  PAIWKLM3M_A=3)  CPEveryDayLimits = 0. 
EXECUTE.

FREQUENCIES VARIABLES=  CPNeverLimits  CPSometimesLimits  CPMostDaysLimits CPEveryDayLimits
  /ORDER=ANALYSIS.


*FUnctional Limitation that affects families among patients with chronic pain. 
 
RECODE PAIAFFM3M_A (SYSMIS=SYSMIS) (1=1) (2=0) (3=0) (4=0) INTO FAMNeverHighImpact.
VARIABLE LABELS  FAMNeverHighImpact 'Pain Never affects family'.
EXECUTE.

RECODE PAIAFFM3M_A (SYSMIS=SYSMIS) (1=0) (2=1) (3=0) (4=0) INTO FAMSometimesHiImpact.
VARIABLE LABELS  FAMSometimesHiImpact 'Pain Sometimes affects family'.
EXECUTE.


RECODE PAIAFFM3M_A (SYSMIS=SYSMIS) (1=0) (2=0) (3=1) (4=0) INTO FAMMostDaysHighImpact.
VARIABLE LABELS  FAMMostDaysHighImpact 'Pain affects family most days'.
EXECUTE.

RECODE PAIAFFM3M_A (SYSMIS=SYSMIS) (1=0) (2=0) (3=0) (4=1) INTO FAMEveryDayHighImpact.
VARIABLE LABELS  FAMEveryDayHighImpact 'Pain affects family every day'.
EXECUTE.


RECODE PAIAFFM3M_A (SYSMIS=SYSMIS) (1=0) (2=0) (3=1) (4=1) INTO HighImpactFAM.
VARIABLE LABELS  HighImpactFAM 'Pain affects family and significant others'.
EXECUTE.


*review binary recoding and ensure accuracy. 
FREQUENCIES VARIABLES= FAMNeverHighImpact FAMSometimesHiImpact FAMMostDaysHighImpact  FAMEveryDayHighImpact
  /ORDER=ANALYSIS.



******************************************************************************************************
*******************************************************************************************************
**CONTROL/ MEDIATING/MODERATING INDEPENDENT VARIABLES******************************************************
******************************************************************************************************
*******************************************************************************************************

RECODE NOTCOV_A (SYSMIS=SYSMIS) (1=1) (2=0) INTO Uninsured.
VARIABLE LABELS  Uninsured 'Not covered by health insurance'.
VALUE LABELS 
    Uninsured
     1 'Not covered by health insurance'
     0 'Covered by health insurance'.
EXECUTE.

FREQUENCIES VARIABLES= uninsured
  /ORDER=ANALYSIS.

RECODE INCWELF_A (SYSMIS=SYSMIS) (1=1) (2=0) INTO PublicAssistance.
VARIABLE LABELS  PublicAssistance 'Received Public Assistance Last Year'.
VALUE LABELS 
    PublicAssistance
     1 'Received public assistance' 
     0 'Did not receive public assistance'.

FREQUENCIES VARIABLES= INCWELF_A
  /ORDER=ANALYSIS.


*recode urban/rural into binaries.
RECODE URBRRL (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=1) INTO Rural.
RECODE URBRRL (SYSMIS=SYSMIS) (1=1) (2=0)(3=0)(4=0) INTO Urban.


*recode female as a binary variable. 
RECODE SEX_A (SYSMIS=SYSMIS) (1=0) (2=1) INTO Female.
VARIABLE LABELS  Female 'Female'.
VALUE LABELS 
    Female
     1 'Female' 
     0 'Male'.

*recode male  as a binary variable. 
RECODE SEX_A (SYSMIS=SYSMIS) (1=1) (2=0) INTO Male.
VARIABLE LABELS  Male 'Male'.
VALUE LABELS 
    Male
     1 'Male'
     0 'Female'.

FREQUENCIES VARIABLES=  Male  Female
  /ORDER=ANALYSIS.

*create genderpain binary variables. 
COMPUTE GenderPain = $sysmis. 
IF ((Male=1) and (ChronicPain_any=1)) GenderPain = 1. 
IF ((Male=0) and (ChronicPain_any=1)) GenderPain = 2.
IF ((Male=1) and (ChronicPain_any=0)) GenderPain = 3. 
IF ((Male=0) and (ChronicPain_any=0)) GenderPain = 4.
Variable Labels GenderPain 'combinations of gender and pain.'.
Value Labels
    GenderPain
    1 'Males with Chronic Pain'
    2 'Females with Chronic Pain '
    3 'Males without chronic Pain '
    4 'Females without Chronic Pain'.
    EXECUTE.
FREQUENCIES VARIABLES=  GenderPain
  /ORDER=ANALYSIS.



RECODE CANEV_A (SYSMIS=SYSMIS) (1=1) (2=0) INTO cancer. 
VARIABLE LABELS  cancer 'Ever had cancer'.
VALUE LABELS
cancer
1 'Has had cancer'
0 'Has not ever had cancer'.
    

*Recode race as binary variables. 
RECODE HISPALLP_A (SYSMIS=SYSMIS) (1=1) (2=0)(3=0)(4=0)(5=0)(6=0)(7=0) INTO Hispanic.
VARIABLE LABELS  Hispanic 'Hispanic'.
EXECUTE.

RECODE HISPALLP_A (SYSMIS=SYSMIS) (1=0) (2=1)(3=0)(4=0)(5=0)(6=0)(7=0) INTO NHW.
VARIABLE LABELS  NHW 'Non-Hispanic White'.
EXECUTE.

RECODE HISPALLP_A (SYSMIS=SYSMIS) (1=0) (2=0)(3=1)(4=0)(5=0)(6=0)(7=0) INTO BlackAfAmer.
VARIABLE LABELS  BlackAfAmer 'Black or African American'.
EXECUTE.

RECODE HISPALLP_A (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=1)(5=0)(6=0)(7=0) INTO Asian.
VARIABLE LABELS  Asian 'Asian'.
EXECUTE.

RECODE HISPALLP_A (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=0)(5=1)(6=0)(7=0) INTO AIANonly.
VARIABLE LABELS  AIANOnly 'AIAN Only'.
EXECUTE.

RECODE HISPALLP_A (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=0)(5=0)(6=1)(7=0) INTO AIAN_multiracialonly.
VARIABLE LABELS  AIAN_multiracialonly 'AIAN and other single and multiple races'.
EXECUTE.

RECODE HISPALLP_A (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=0)(5=1)(6=1)(7=0) INTO AIAN_all.
VARIABLE LABELS  AIAN_all 'AIAN'.
EXECUTE.

RECODE HISPALLP_A (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=0)(5=0)(6=0)(7=1) INTO Other.
VARIABLE LABELS  Other 'Other Single and Multiple Races'.
EXECUTE.


recode MARITAL_A (SYSMIS=SYSMIS) (1=1) (2=1)(3=0) into CurrentlyHasPartner.
Execute. 


recode MARITAL_A (SYSMIS=SYSMIS) (1=0) (2=0)(3=1) into NOcurrentPartner.
Execute. 
 
recode MEDDL12M_A (SYSMIS=SYSMIS) (1=1) (2=0) into DelayedMedicalCareDueToCost.
Execute. 
 
recode WRKHLTHFC_A (SYSMIS=SYSMIS) (1=1) (2=0) into WorkInHealthCare.
Execute. 
 
recode PCNTLT18TC (SYSMIS=SYSMIS) (0=0) (1=1)  (2=1)  (3=1) into ChildAtHome.
Execute.


*Categorical and binary Age variables. 
COMPUTE AgeCat = $sysmis. 
execute. 
IF (AGEP_A le 24)  AgeCat = 1. 
IF (AGEP_A ge 25) and (AGEP_A le 34)  AgeCat = 2. 
IF (AGEP_A ge 35) and (AGEP_A le 44)  AgeCat = 3. 
IF (AGEP_A ge 45) and (AGEP_A le 54)  AgeCat = 4. 
IF (AGEP_A ge 55) and (AGEP_A le 64)  AgeCat = 5. 
IF (AGEP_A ge 65) and (AGEP_A le 74)  AgeCat = 6. 
IF (AGEP_A ge 75) and (AGEP_A le 84)  AgeCat = 7. 
IF (AGEP_A EQ 85)  AgeCat = 8. 
execute. 
 

VARIABLE LABELS  AgeCat 'Age'.
VALUE LABELS
AgeCat
1 '18-24'
2 '25-34'
3 '35-44'
4 '45-54'
5 '55-64'
6 '65-74'
7 '75-84'
8 '85+'.
Execute.
    
RECODE AgeCat (SYSMIS=SYSMIS) (1=1) (2=0)(3=0)(4=0)(5=0) (6=0)(7=0)(8=0) INTO age18_24.
VARIABLE LABELS age18_24  '18-24'.
EXECUTE.

RECODE AgeCat (SYSMIS=SYSMIS) (1=0) (2=1)(3=0)(4=0)(5=0) (6=0)(7=0)(8=0) INTO age25_34.
VARIABLE LABELS age25_34  '25-34'.
EXECUTE.

RECODE AgeCat (SYSMIS=SYSMIS) (1=0) (2=0)(3=1)(4=0)(5=0) (6=0)(7=0)(8=0) INTO age35_44.
VARIABLE LABELS age35_44 '35-44'.
EXECUTE.

RECODE AgeCat (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=1)(5=0) (6=0)(7=0)(8=0) INTO age45_54.
VARIABLE LABELS age45_54  '45-54'.
EXECUTE.

RECODE AgeCat (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=0)(5=1) (6=0)(7=0)(8=0) INTO age55_64.
VARIABLE LABELS age55_64  '55-64'.
EXECUTE.

RECODE AgeCat (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=0)(5=0) (6=1)(7=0)(8=0) INTO age65_74.
VARIABLE LABELS age65_74 '65-74'.
EXECUTE.

RECODE AgeCat (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=0)(5=0) (6=0)(7=1)(8=0) INTO age75_84.
VARIABLE LABELS age75_84  '75-84'.
EXECUTE.

RECODE AgeCat (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=0)(5=0) (6=0)(7=0)(8=1) INTO age85plus.
VARIABLE LABELS age85plus '85+'.
EXECUTE.

FREQUENCIES VARIABLES=AgeCat age18_24 age25_34 age35_44  age45_54 age55_64 age65_74 age75_84 age85plus 
  /ORDER=ANALYSIS.

*Categorical and binary Education variables. 
COMPUTE EducCat = $sysmis. 
execute. 
IF (EDUC_A le 2)  EducCat = 1. 
IF (EDUC_A ge 3) and (EDUC_A le 4)  EducCat = 2. 
IF (EDUC_A ge 5) and (EDUC_A le 7)  EducCat = 3. 
IF (EDUC_A = 8)  EducCat = 4. 
IF (EDUC_A ge 9) and (EDUC_A le 11)  EducCat = 5. 
execute.

VARIABLE LABELS  EducCat 'Education Level'.
VALUE LABELS
EducCat
1 'Less than high school'
2 'Graduated high school or GED'
3 'Some college or Associates degree'
4 'Bachelors Degree'
5 'Professional or Graduate degree'.
Execute.
    
RECODE EducCat (SYSMIS=SYSMIS) (1=1) (2=0)(3=0)(4=0)(5=0) INTO NoHighSchool.
VARIABLE LABELS NoHighSchool  'Less than high school'.
EXECUTE.

RECODE EducCat (SYSMIS=SYSMIS) (1=0) (2=1)(3=0)(4=0)(5=0) INTO HighSchoolGrad.
VARIABLE LABELS HighSchoolGrad  'Graduated high school or GED'.
EXECUTE.

RECODE EducCat (SYSMIS=SYSMIS) (1=0) (2=0)(3=1)(4=0)(5=0)INTO SomeCollege.
VARIABLE LABELS SomeCollege  'Some college or Associates degree'.
EXECUTE.

RECODE EducCat (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=1)(5=0) INTO Bachelors.
VARIABLE LABELS Bachelors  'Bachelors Degreel'.
EXECUTE.

RECODE EducCat (SYSMIS=SYSMIS) (1=0) (2=0)(3=0)(4=0)(5=1) INTO GradSchool.
VARIABLE LABELS GradSchool  'Professional or Graduate degree'.
EXECUTE.

FREQUENCIES VARIABLES=EducCat  NoHighSchool HighSchoolGrad SomeCollege Bachelors GradSchool
  /ORDER=ANALYSIS.

