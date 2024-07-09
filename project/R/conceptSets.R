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
    )
  )
)
conceptSets$conceptSets <- list()
for (cs in names(conceptSets$concepts)) {
  conceptSets$conceptSets[[cs]] <- cs(descendants(conceptSets$concepts[[cs]]), name = cs)
}

cat("Sourced concept sets")
