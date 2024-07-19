library(Capr)

## Define list of concepts to use for concept sets
# include all descendant concepts
conceptSets <- list(
  concepts = list(
    
    # laboratory tests
    labTests = c(  # 5.13
      4298431,  # source: Immature WBC Count /// concept: White blood cell count
      3007670,  # source: Neutrophil Count /// concept: Neutrophil Ab [Units/volume] in Serum
      40762351,  # source: Hemoglobin /// concept: Hemoglobin [Moles/volume] in Blood
      37393863,  # source: Platelet Count /// concept: Platelet count
      3009542,  # source: Hematocrit /// concept: Hematocrit [Volume Fraction] of Blood
      3034426,  # source: INR Prothrombin Time /// concept: Prothrombin time (PT)
      3047181,  # source: Lactate /// concept: Lactate [Moles/volume] in Blood
      3051825,  # source: Creatinine /// concept: Creatinine [Mass/volume] in Blood
      3024561,  # source: Albumin /// concept: Albumin [Mass/volume] in Serum or Plasma
      3024641,  # source: Blood urea nitrogen /// concept: Urea nitrogen [Moles/volume] in Serum or Plasma
      3013826,  # source: Glucose /// concept: Glucose [Moles/volume] in Serum or Plasma
      3006140,  # source: Bilirubin /// concept: Bilirubin.total [Moles/volume] in Serum or Plasma
      3019550,  # source: Sodium /// concept: Sodium [Moles/volume] in Serum or Plasma
      3005456,  # source: Potassium /// concept: Potassium [Moles/volume] in Blood
      3033836,  # source: Magnesium /// concept: Magnesium [Moles/volume] in Blood
      3018572,  # source: Chloride /// concept: Chloride [Moles/volume] in Blood
      3015377,  # source: Calcium /// concept: Calcium [Moles/volume] in Serum or Plasma
      3003458,  # source: Phosphate /// concept: Phosphate [Moles/volume] in Serum or Plasma
      3020460,  # source: CRP /// concept: C reactive protein [Mass/volume] in Serum or Plasma
      3006923,  # source: ALT /// concept: Alanine aminotransferase [Enzymatic activity/volume] in Serum or Plasma
      3013721,  # source: AST /// concept: Aspartate aminotransferase [Enzymatic activity/volume] in Serum or Plasma
      37392672,  # source: pH (venous) /// concept: Blood venous pH
      37399161,  # source: pH (arterial) /// concept: Blood arterial pH
      3021447,  # source: pCO2 (venous) /// concept: Carbon dioxide [Partial pressure] in Venous blood
      3027946,  # source: pCO2 (arterial) /// concept: Carbon dioxide [Partial pressure] in Arterial blood
      3024354,  # source: pO2 (venous) /// concept: Oxygen [Partial pressure] in Venous blood
      3027801,  # source: pO2 (arterial) /// concept: Oxygen [Partial pressure] in Arterial blood
      3027273,  # source: Bicarbonate (venous) /// concept: Bicarbonate [Moles/volume] in Venous blood
      3008152  # source: Bicarbonate (arterial) /// concept: Bicarbonate [Moles/volume] in Arterial blood
    ),
    
    # cardiac complications
    cardiacComplications = c(
      4324124,  # source: LCOS - renal support (CVVH/ PD) concept1 /// concept: Peritoneal dialysis
      4051330,  # source: LCOS - renal support (CVVH/ PD) concept2 /// concept: Continuous venovenous hemofiltration
      4185565,  # source: LCOS - renal support (CVVH/ PD) concept3 /// concept: Low cardiac output syndrome
      321042  # source: LCOS - cardiac arrest /// concept: Cardiac arrest
    ),
    
    # intubation
    intubation = c(
      4202832  # source: Intubation /// concept: Intubation
    ),
    
    # cardiac procedures
    cardiacProcedures = c(
      4296790, # source: Transposition of coronary artery NEC /// concept: Transposition of coronary artery
      4137127, # source: Unspecified other transplantation of heart/allotransplantation of heart NEC /// concept: Transplantation of heart
      44789857, # source: Total cavopulmonary connection with extracardiac inferior caval vein topulmonary artery conduit /// concept: Total cavopulmonary connection with extracardiac inferior caval vein to pulmonary artery conduit
      4019950, # source: Revision of cardiac conduit nec/creation of valved conduit between Right ventricle of heart and Pulmonary artery/replacement of valved cardiac conduit/ replacement of pulmonary valve nec /// concept: Revision of valved cardiac conduit
      4178479, # source: Revision of cardiac conduit nec/creation of valved conduit between Right ventricle of heart and Pulmonary artery/replacement of valved cardiac conduit/ replacement of pulmonary valve nec /// concept: Replacement of valved cardiac conduit
      4339184, # source: Revision of cardiac conduit nec/creation of valved conduit between Right ventricle of heart and Pulmonary artery/replacement of valved cardiac conduit/ replacement of pulmonary valve nec /// concept: Replacement of pulmonary valve
      4203153,  # source: Replacement of mitral valve NEC /// concept: Replacement of mitral valve
      4232476, # source: Repair of defect of interventricular septum using prosthetic patch/unspecified repair of defect of interventricular septum /// concept: Repair of ventricular septal defect with prosthesis
      4293619, # source: Tricuspid valve repair NEC /// concept: Repair of tricuspid valve
      4017751, # source: Unspecified correction of total anomalous pulmonary venous connection /// concept: Repair of total anomalous pulmonary venous connection
      4019929, # source: Repair of tetralogy of fallot using transannular patch /// concept: Repair of tetralogy of Fallot
      4020508, # source: Correction of persistent sinus venosus /// concept: Repair of sinus venosus
      4018926, # source: Other specified : repair of pulmonary artery /// concept: Repair of pulmonary artery
      4018441, # source: Correction of partial anomalous pulmonary venous drainage /// concept: Repair of partial anomalous pulmonary venous connection
      4187380, # source: Primary repair of defect of atrioventricular septum NEC/repair of persistent ostium primum/repair of persistent ostium primum /// concept: Repair of ostium primum defect
      4312194, # source: Aortic valve repair NEC /// concept: Repair of heart valve
      4049979, # source: Repair of double outlet right ventricle /// concept: Repair of double outlet right ventricle
      4021725,  # source: Removal of band from pulmonary artery /// concept: Removal of band from pulmonary artery
      44790092, # source: Relief of left ventricular outflow tract obstruction /// concept: Relief of left ventricular outflow tract obstruction
      4019028, # source: Release of vascular ring of aorta /// concept: Release of vascular ring of aorta
      40486525, # source: Primary repair of defect of atrioventricular septum NEC/repair of persistent ostium primum/repair of persistent ostium primum /// concept: Primary repair of defect of atrioventricular septum
      4217615, # source: Plication of diaphragm /// concept: Plication of diaphragm
      4020812, # source: Plastic repair of aorta and end to end anastomosis of aorta /// concept: Plastic repair of aorta and end-to-end anastomosis of aorta
      4019026, # source: Other specified: plastic repair of aorta /// concept: Plastic repair of aorta
      44510968  # source: Other specified other operations on ventricles of heart /// concept: Other specified other operations on ventricles of heart
    ),

    # 
    vitalSigns = c(
      3027018, # source: Age-dependent HR /// concept: Heart rate
      3024171, # source: Age-dependent RR /// concept: Respiratory rate
      3004249, # source: SBP (Systolic Blood Pressure) /// concept: Systolic blood pressure
      3012888, # source: DBP (Dyastolic Blood Pressure) /// concept: Diastolic blood pressure
      4020553 # source: SpO2 /// concept: Oxygen saturation measurement
    ),

    # 5.16 HSJD clinical
    HSJDClinical = c(
      3025315, # source: Weight /// concept: Body weight
      3036277, # source: Height /// concept: Body height
      4201235, # source: Body Surface Area (BSA) - Combination of weight and height /// concept: body surface area
      4275564 # source: Cardiac surgeries (Interventions) /// concept: Operation on heart
    ), 
    
    # 5.17 HSJD Surgery
    HSJDSurgery = c(
      40490494, # source: STAT/EACTS score of surgery /// concept: Society of Thoracic Surgeons risk calculator
      4301351, # source: Surgery start datetime, end datetime /// concept: Surgical procedure
      4336464, # source: Cardiac bypass start datetime, end datetime /// concept: Coronary artery bypass graft
      4201547, # source: Cross-clamping start datetime, end datetime /// concept: Placement of arterial cross clamp
      37152781, # source: Deep hypothermic circulatory arrest start datetime, end datetime /// concept: Hypothermic circulatory arrest
      4272324, # source: Antegrade Cerebral Perfusion start datetime, end datetime /// concept: Cardiopulmonary bypass operation
      4150627 # source: Extubation event /// concept: Removal of endotracheal tube
    ),

    # 5.18 HSJD Hospitalisations 
    HSJDHospitalisation = c(
      44803020, # source: Admission reason /// concept: Primary reason for admission
      42539650, # source: Vasoactive infusion start datetime, end datetime /// concept: Administration of intravenous vasoactive drug
      4179206, # source: Postop central venous catheter in/start datetime, out/end datetime /// concept: Central venous catheter
      37158404, # source: Invasive Mechanical Ventilation LOS /// concept: Invasive mechanical ventilation
      4177224 # source: Non-Invasive Mechanical Ventilation LOS /// concept: Non-invasive ventilation
    ),

    # 5.19 HSJD Complications
    HSJDcomplications = c(
      4202832, # source: intubation /// concept: Intubation
      4275564, # source: Cardiac surgeries (interventions) /// concept: Operation on heart
      4044892, # source: Sternum reopening (procedure undertaken) /// concept: Procedure on sternum
      4046868, # source: Vocal Cord Dysfunction /// concept: Vocal cord dysfunction
      4275136, # source: Diaphragmatic Paralysis /// concept: Paralysis of diaphragm
      253796, # source: Pneumothorax /// concept: Pneumothorax
      44783799, # source: Tracheostomy /// concept: Exteriorization of trachea
      44784217, # source: Arrhythmia diagnosis (tachyarrhythmia, bradyarrhythmia, atrial, ventricular etc.) /// concept: Cardiac arrhythmia
      315643, # source: Arrhythmia diagnosis (tachyarrhythmia, bradyarrhythmia, atrial, ventricular etc.) /// concept: Tachyarrhythmia
      4228448, # source: Arrhythmia diagnosis (tachyarrhythmia, bradyarrhythmia, atrial, ventricular etc.) /// concept: Bradyarrhythmia
      4068155, # source: Arrhythmia diagnosis (tachyarrhythmia, bradyarrhythmia, atrial, ventricular etc.) /// concept: Atrial arrhythmia
      4185572, # source: Arrhythmia diagnosis (tachyarrhythmia, bradyarrhythmia, atrial, ventricular etc.) /// concept: Ventricular arrhythmia
      46234437, # source: Drugs administered; Arrhythmia therapy (drugs): IV anthyarritmics (ivabradine, amiodarone, flecainide) /// concept: ivabradine
      1309944, # source: Drugs administered; Arrhythmia therapy (drugs): IV anthyarritmics (ivabradine, amiodarone, flecainide) /// concept: amiodarone
      1354860, # source: Drugs administered; Arrhythmia therapy (drugs): IV anthyarritmics (ivabradine, amiodarone, flecainide) /// concept: flecainide
      4353741, # source: Arrhythmia therapy (procedures): Cardioversion, Rapid atrial pacing, temporary pacing, permanent pacing /// concept: Cardioversion
      4117045, # source: Arrhythmia therapy (procedures): Cardioversion, Rapid atrial pacing, temporary pacing, permanent pacing /// concept: Atrial overdrive pacing
      4049398, # source: Arrhythmia therapy (procedures): Cardioversion, Rapid atrial pacing, temporary pacing, permanent pacing /// concept: Temporary cardiac pacemaker procedure
      4051940, # source: Arrhythmia therapy (procedures): Cardioversion, Rapid atrial pacing, temporary pacing, permanent pacing /// concept: Permanent cardiac pacemaker procedure
      609312 # source: Listed for heart transplantation /// concept: Awaiting transplantation of heart
      ),

    # 5.28 Patient characteristics 
    UC3PateintCharacteristics = c(
      434007, # source: hemophilia A diagnosis /// concept: hereditary factor VIII deficiency disease
      4094223, # source: hemophilia A subtype/ severity /// concept: mild hereditary factor VIII deficiency disease
      4140661, # source: hemophilia A subtype/ severity /// concept: moderate hereditary factor VIII deficiency disease
      4056830, # source: hemophilia A subtype/ severity /// concept: severe hereditary factor VIII deficiency disease
      3003694, # source: blood group /// concept: ABO and Rh group [Type] in Blood
      37393608, # source: factor VIII inhibitor status /// concept: factor VIII inhibitor activity
      4126681, # source: factor VIII inhibitor status /// concept: detected
      9190 # source: factor VIII inhibitor status /// concept: not detected
    ),

    # 5.29 Lab Measurements
    UC3LabMeasurements = c(
      3024942, # source: factor VIII inhibitor titer /// concept: coagulation factor VIII inhibitor [Units/volume] in platelet poor plasma by coagulation assay
      3022520, # source: factor VIII activity measurement /// concept: coagulation factor VIII activated [Units/volume] in platelet poor plasma by coagulation assay
      3011832, # source: factor VIII activity measurement /// concept: coagulation factor VIII activity [Units/volume] in platelet poor plasma by Chromogenic assay
      43534000, # source: Von Willebrand factor activity measurement /// concept: von Willebrand factor (vWf) activity [Units/volume] in platelet poor plasma by Immunoassay
      3002124, # source: Von Willebrand factor antigen measurement /// concept: von Willebrand factor (vWf) Ag [Units/volume] in platelet poor plasma by Immunoassay
      3023693, # source: Von Willebrand factor propeptide measurement /// concept: von Willebrand factor (vWf) multimers in platelet poor plasma by Immunoblot
      3042349, # source: Von Willebrand factor propeptide measurement /// concept: von Willebrand factor (vWf) cleaving protease inhibitor [Units/volume] in platelet poor plasma
      4175016, # source: activated partial thromboplastin time (APTT) /// concept: partial thromboplastin time, activated
      3034426, # source: Prothrombin time (PT) /// concept: prothrombin time (PT)
      4267147, # source: platelet count /// concept: platelet count
      3009542, # source: hematocrit /// concept: Hematocrit [Volume Fraction] of Blood
      3016407, # source: fibrinogen /// concept: fibrinogen [mass/volume] in platelet poor plasma by coagulation assay
      3006923, # source: ALT /// concept: alanine aminotransferase [enzymatic activity/volume] in serum or plasma
      3013721 # source: AST /// concept: aspartate aminotransferase [enzymatic activity/volume] in serum or plasma
    ),

    # 5.30 Treatments 
    UC3Treatments = c(
      1352213, # source: factor VIII /// concept: factor VIII (umbrella term)
      793042, # source: factor VIII /// concept: emicizumab (umbrella term)
      40492862, # source: factor VIII dose is continuous infusion or bolus dose /// concept: mode of drug administration
      4129275, # source: factor VIII dose is continuous infusion or bolus dose /// concept: continuous infusion
      4265597, # source: factor VIII dose is continuous infusion or bolus dose /// concept: by bolus infusions
      1517070 # source: desmopressin /// concept: desmopressin
    ),

    # 5.31 Surgery-specific parameters
    UC3Surgery = c(
      4172515, # source: Medical/ surgical procedure /// concept: Therapeutic procedure
      967823, # source: NaCl administration during surgery /// concept: sodium chloride
      4028665, # source: plasma administration during surgery /// concept: plasma transfusion
      4160439, # source: datetime start anesthesia /// concept: Administration of anesthesia
      4308716, # source: blood loss during surgery /// concept: intraoperative hemorrhage
      37017589 # source: blood transfusion during surgery /// concept: Bleeding during surgery requiring transfusion
    )
  )
)
conceptSets$conceptSets <- list()
for (cs in names(conceptSets$concepts)) {
  conceptSets$conceptSets[[cs]] <- cs(descendants(conceptSets$concepts[[cs]]), name = cs)
}

cat("Sourced concept sets")
