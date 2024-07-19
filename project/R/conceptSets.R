library(Capr)

## Define list of concepts to use for concept sets
# include all descendant concepts
conceptSets <- list(
  concepts = list(
    # 5.3 VIS score medications
    visScoreMedications = c(
      1337720, # source: dobutamine /// concept: dobutamine
      1337860, # source: dopamine /// concept: dopamine
      1343916, # source: epinephrine /// concept: epinephrine
      1368671, # source: milrinone /// concept: milrinone
      1321341, # source: norepinephrin /// concept: norepinephrine
      1507835  # source: vasopressin /// concept: vasopressin (USP)
    ),
    
    # 5.4 vasopressors and inotropes
    vasopressors_inotropes = c(
      1337720, # source: Dobutamine /// concept: dobutamine
      1337860, # source: Dopamine /// concept: dopamine
      1143374, # source: Ephedrine /// concept: ephedrine
      1343916, # source: Epinephrine /// concept: epinephrine
      1183554, # source: Isoproterenol /// concept: isoproterenol
      40173184, # source: Levosimendan /// concept: levosimendan
      1368671, # source: Milrinone /// concept: milrinone
      1321341, # source: Norepinephrine /// concept: norepinephrine
      1135766, # source: Phenylephrine /// concept: phenylephrine
      19119253, # source: Terlipressin /// concept: terlipressin
      1507835  # source: Vasopressin /// concept: vasopressin (USP)
    ),
    
    # 5.5 immunosuppressants
    immunosuppressants = c(
      4273629, # source: Chemotherapy /// concept: Chemotherapy
      1518254, # source: Dexamethasone /// concept: dexamethasone
      975125, # source: Hydrocortisone /// concept: hydrocortisone
      1506270, # source: Methylprednisolone /// concept: methylprednisolone
      21603754, # source: Monoclonal Antibodies /// concept: Monoclonal antibodies
      1550557 # source: Prednisolone /// concept: prednisolone
    ),
    
    # 5.6 antibiotic medications
    antibiotics = c(
      1713332, # source: Amoxicillin /// concept: amoxicillin
      1759842, # source: Amoxicillin clavulanate /// concept: clavulanate
      1717327, # source: Ampicillin /// concept: ampicillin
      1734104, # source: Azithromycin /// concept: azithromycin
      1836430, # source: Trimethoprim-Sulfamethoxazole /// concept: sulfamethoxazole
      1705674, # source: Trimethoprim-Sulfamethoxazole /// concept: trimethoprim
      1836948, # source: Tetracycline /// concept: tetracycline
      902722, # source: Tobramycin /// concept: tobramycin
      1707687, # source: Vancomycin /// concept: vancomycin
      19010400, # source: Fusidic acid /// concept: fusidate
      45892419, # source: Gentamicin /// concept: gentamicin
      45892599, # source: Ceftolozane-Tazovactam /// concept: ceftolozane
      46221507, # source: Ceftazidime-Avibactam /// concept: avibactam
      1736887, # source: Linezolid /// concept: linezolid
      1748975, # source: Cefepime /// concept: cefepime
      1750500, # source: Clarithromycin /// concept: clarithromycin
      1769535, # source: Cefadroxil /// concept: cefadroxil
      1771162, # source: Cefazolin /// concept: cefazolin
      1774470, # source: Cefotaxime /// concept: cefotaxime
      1776684, # source: Ceftazidime /// concept: ceftazidime
      1778162, # source: Cefuroxime /// concept: cefuroxime
      1786617, # source: Daptomycin /// concept: daptomycin
      1797513, # source: Ciprofloxacin /// concept: ciprofloxacin
      997881, # source: Clindamycin /// concept: clindamycin
      901845, # source: Colistin /// concept: colistin
      1709170, # source: Meropenem /// concept: meropenem
      1717963, # source: Ertapenem /// concept: ertapenem
      1518254, # source: Dexamethasone /// concept: dexamethasone
      1741122, # source: Piperacillin-Tazobactam /// concept: tazobactam
      1746114, # source: Piperacillin-Tazobactam /// concept: piperacillin
      1746940, # source: Erythromycin /// concept: erythromycin
      951511, # source: Mupirocin /// concept: mupirocin
      956653, # source: Fosfomycin /// concept: fosfomycin
      975125, # source: Hydrocortisone /// concept: hydrocortisone
      1778262, # source: Imipenem /// concept: imipenem
      19078399, # source: Teicoplanin /// concept: teicoplanin
      1790868, # source: Amikacin /// concept: amikacin
      1506270, # source: Methylprednisolone /// concept: methylprednisolone
      920293, # source: Nitrofurantoin /// concept: nitrofurantoin
      1721543, # source: Norfloxacin /// concept: norfloxacin
      1724703, # source: Oxacillin /// concept: oxacillin
      1728416, # source: Penicillin /// concept: penicillin G
      1742253, # source: Levofloxacin /// concept: levofloxacin
      1550557, # source: Prednisolone /// concept: prednisolone
      1763204 # source: Rifampicin /// concept: rifampin
    ),
    
    # 5.7 antifungal medications
    antifungals = c(
      1714277, # source: Voriconazole /// concept: voriconazole
      35606695, # source: Isovuconazole /// concept: isavuconazole
      19018013, # source: Micafungin /// concept: micafungin
      19026450, # source: Andiulafungin /// concept: anidulafungin
      1754994 # source: Fluconazole /// concept: fluconazole
    ),
    
    # 5.8 antibiotic resistance
    antibioticResistance = c(
      4019195, # source: MRSA /// concept: Methicillin resistant Staphylococcus aureus
      4257547, # source: ESBL /// concept: Extended spectrum beta-lactamase producing bacteria
      37017134, # source: Drug-resistant /// concept: Multidrug-resistant bacteria
      1707687, # source: Vancomycin-resistant /// concept: vancomycin
      997881, # source: Clindamycin-resistant /// concept: clindamycin
      1746940 # source: Erythromycin-resistant /// concept: erythromycin
    ),
    
    # 5.9 cardiac surgery
    cardiacSurgery = c(
      4049734, # source: Application of band to pulmonary artery /// concept: Banding of pulmonary artery
      4019929, # source: Unspecified repair of tetralogy of fallot /// concept: Repair of tetralogy of Fallot
      4019932, # source: Repositioning of transposed great arteries /// concept: Arterial switch operation
      4017751, # source: Unspecified correction of total anomalous pulmonary venous connection /// concept: Repair of total anomalous pulmonary venous connection
      4020376, # source: Primary repair of defect of interatrial septum NEC/repair of defect of interatrial septum using pericardial patch /// concept: Closure of defect of interatrial septum using pericardial patch
      4020506, # source: Revision of cardiac conduit nec/creation of valved conduit between Right ventricle of heart and Pulmonary artery/replacement of valved cardiac conduit/ replacement of pulmonary valve nec /// concept: Creation of valved conduit between right ventricle of heart and pulmonary artery
      4019950, # source: Revision of cardiac conduit nec/creation of valved conduit between Right ventricle of heart and Pulmonary artery/replacement of valved cardiac conduit/ replacement of pulmonary valve nec /// concept: Revision of valved cardiac conduit
      4020508, # source: Correction of persistent sinus venosus /// concept: Repair of sinus venosus
      4018441, # source: Correction of partial anomalous pulmonary venous drainage /// concept: Repair of partial anomalous pulmonary venous connection
      4020520, # source: Open aortic valvotomy /// concept: Open aortic valvotomy
      4018747, # source: Repair of subaortic stenosis /// concept: Operations on the left ventricular outflow tract
      4019233, # source: Unspecified creation of shunt to pulmonary artery from subclavian artery using interposition tube pr /// concept: Creation of shunt from subclavian artery to pulmonary artery using interposition tube prosthesis
      4019237, # source: Creation of anastomosis to pulmonary artery from vena cava /// concept: Anastomosis of vena cava to pulmonary artery
      4018926, # source: Other specified : repair of pulmonary artery /// concept: Repair of pulmonary artery
      4021725, # source: Removal of band from pulmonary artery /// concept: Removal of band from pulmonary artery
      4019026, # source: Other specified: plastic repair of aorta /// concept: Plastic repair of aorta
      4020812, # source: Plastic repair of aorta and end to end anastomosis of aorta /// concept: Plastic repair of aorta and end-to-end anastomosis of aorta
      4019028, # source: Release of vascular ring of aorta /// concept: Release of vascular ring of aorta
      44790092, # source: Relief of left ventricular outflow tract obstruction /// concept: Relief of left ventricular outflow tract obstruction
      44789857, # source: Total cavopulmonary connection with extracardiac inferior caval vein topulmonary artery conduit /// concept: Total cavopulmonary connection with extracardiac inferior caval vein to pulmonary artery conduit
      4336751, # source: Unspecified other transplantation of heart/allotransplantation of heart NEC /// concept: Allotransplant of heart
      4049979, # source: Repair of double outlet right ventricle /// concept: Repair of double outlet right ventricle
      4050114, # source: Closure of patent ductus arteriosus NEC /// concept: Closure of ductus arteriosus with clip
      4052536, # source: Extracorporeal membrane oxygenation /// concept: Extracorporeal membrane oxygenation
      44790415, # source: Aortic root pulmonary valve autograft with right vent to pulmonary artery valved conduit/ aortic root pulmonary valve autograft with right vent to pulmonary artery aortoventriculoplasty /// concept: Aortic root replacement using pulmonary valve autograft with right ventricle to pulmonary artery valved conduit and aortoventriculoplasty
      4144921, # source: Implantation of cardiac pacemaker system NEC /// concept: Implantation of cardiac pacemaker
      4137127, # source: Unspecified other transplantation of heart/allotransplantation of heart NEC /// concept: Transplantation of heart
      44793133, # source: Aortopulmonary reconstruction with systemic to pulmonary arterial shunt /// concept: Aortopulmonary reconstruction with systemic to pulmonary arterial shunt
      4293619, # source: Tricuspid valve repair NEC /// concept: Repair of tricuspid valve
      4296790, # source: Transposition of coronary artery NEC /// concept: Transposition of coronary artery
      4139214, # source: Open implantation of ventricular assist device /// concept: Open implantation of cardiac ventricular assist device
      4178479, # source: Revision of cardiac conduit nec/creation of valved conduit between Right ventricle of heart and Pulmonary artery/replacement of valved cardiac conduit/ replacement of pulmonary valve nec /// concept: Replacement of valved cardiac conduit
      40486525, # source: Primary repair of defect of atrioventricular septum NEC/repair of persistent ostium primum/repair of persistent ostium primum /// concept: Primary repair of defect of atrioventricular septum
      4187380, # source: Primary repair of defect of atrioventricular septum NEC/repair of persistent ostium primum/repair of persistent ostium primum /// concept: Repair of ostium primum defect
      4199899, # source: Unspecified repair of defect of interventricular septum /// concept: Closure of ventricular septal defect
      4203153, # source: Replacement of mitral valve NEC /// concept: Replacement of mitral valve
      4217615, # source: Plication of diaphragm /// concept: Plication of diaphragm
      4308136, # source: Repair of tetralogy of fallot using transannular patch /// concept: Complete repair of tetralogy of Fallot with transannular patch
      4312194, # source: Aortic valve repair NEC /// concept: Repair of heart valve
      4339184, # source: Revision of cardiac conduit nec/creation of valved conduit between Right ventricle of heart and Pulmonary artery/replacement of valved cardiac conduit/ replacement of pulmonary valve nec /// concept: Replacement of pulmonary valve
      4232476 # source: Repair of defect of interventricular septum using prosthetic patch/unspecified repair of defect of interventricular septum /// concept: Repair of ventricular septal defect with prosthesis
    ),
    
    # 5.10 cardiac radiology
    cardiacRadiology = c(
      4019824, # source: US transoesophageal echocardiogram (toe) /// concept: Transesophageal echocardiography
      4032404, # source: IR bronchoscopy /// concept: Bronchoscopy
      4065416, # source: Exercise test (non-CPET) /// concept: Exercise tolerance test
      4345925, # source: FL video swallow /// concept: Videofluoroscopy swallow
      4083106, # source: US cranial contents /// concept: US scan of head
      4083108, # source: US neck /// concept: US scan of neck
      4082979, # source: MRI head /// concept: MRI of head
      4093436, # source: US thorax and pleural cavity /// concept: Ultrasonography of thorax
      4125350, # source: CT head /// concept: CT of head
      4125530, # source: US urinary tract /// concept: US urinary tract
      44802640, # source: MRI cardiac complex congenital /// concept: MRI study for cardiac congenital anomaly
      4163872, # source: XR chest /// concept: Plain chest X-ray
      4167029, # source: US doppler groin both /// concept: Ultrasonography of inguinal region
      4167052, # source: US doppler renal both /// concept: Doppler ultrasonography of kidney
      4305221, # source: US abdomen and pelvis/ US abdomen /// concept: US scan of abdomen and pelvis
      4303522, # source: US diaphragmatic region /// concept: US scan of diaphragm
      4306317, # source: CT heart with contrast/ CT cardiac angiogram coronary/ CT cardiac gated with contrast /// concept: CT angiography of coronary artery with contrast
      4169275, # source: XR chest and abdomen /// concept: X-ray of chest and abdomen
      4322380, # source: IR PICC line insertion /// concept: Insertion of peripherally inserted central catheter
      4140473, # source: 24h holter monitor /// concept: Holter extended electrocardiographic recording
      4329508, # source: US thorax and pleural cavity /// concept: Ultrasonography of pleural cavity
      4335392, # source: US doppler lower limb veins both /// concept: Doppler ultrasonography of vein of lower limb
      4203365, # source: Epicardial echocardiogram /// concept: Epicardial echocardiography
      4335825, # source: US transthoracic echocardiogram/ us transthoracic echocardiogram (pre- admission)/ US transthoracic echocardiogram (sedated) /// concept: Transthoracic echocardiography
      40482732, # source: IR tunnelled central venous line insertion /// concept: Insertion of tunneled venous catheter
      40489841, # source: US doppler jugular vein both /// concept: Doppler ultrasonography of jugular vein
      40492338, # source: Cardiopulmonary exercise test (CPET) /// concept: Cardiopulmonary exercise test
      40488431, # source: Pacemaker/ICD interrogation (in clinic)/ pacemaker/ICD interrogation (other) /// concept: Interrogation of cardiac pacemaker
      42873079, # source: IR tunnelled central venous line removal /// concept: Removal of tunneled central venous catheter
      4261497, # source: US abdomen and pelvis/ US abdomen /// concept: Ultrasonography of abdomen
      4205144, # source: EEG routine portable /// concept: Portable electroencephalogram
      4181917, # source: EEG routine /// concept: Electroencephalogram
      4264477, # source: XR abdomen /// concept: Diagnostic radiography of abdomen
      45764527, # source: 24h holter monitor /// concept: Electrocardiographic Holter analyzer
      45765560, # source: Event monitor /// concept: Cardiovascular monitor
      4327032, # source: CT thorax with contrast /// concept: CT of thorax with contrast
      35622931, # source: US doppler /// concept: Doppler ultrasound
      44813863, # source: US vocal cord /// concept: Ultrasonography of vocal cord
      4312208, # source: IR bronchogram /// concept: Contrast bronchogram
      4235141 # source: Pacemaker/ICD device check - remote patient initiated /// concept: Check artificial pacemaker
    ),
    
    # 5.11 cardiac length of stay
    cardiac_LOS = c(
      4123933, # source: ICU admission (start datetime and end datetime) /// concept: Admission to pediatric intensive care unit
      8715, # source: Total hospital admission (start datetime and end datetime) /// concept: Hospital admission
      4161811, # source: HDU admission (start datetime and end datetime) /// concept: Admission to high dependency unit
      36675203 # source: Cardiology ward admission (start datetime and end datetime) /// concept: Admission to pediatric cardiology department
    ),
    
    # 5.12 additional variables
    additional = c(
      4095105, # source: Base Excess /// concept: Base deficit measurement
      4123933, # source: Elective ICU admission /// concept: Admission to pediatric intensive care unit
      4234469, # source: Diagnoses /// concept: Diagnosis
      40493026, # source: Mechanical ventilation /// concept: Mechanical ventilator
      44803020, # source: Reason for ICU admission: (Main reason for ICU admission is not recovery from surgery or a procedure, Recovery from a bypass cardiac procedure, recovery from a non-bypass cardiac procedure, recovery from non-cardiac procedure) /// concept: Primary reason for admission
      4209008, # source: Pupillary reaction /// concept: Pupillary function
      260134, # source: Low Risk Diagnoses: None, asthma, bronchiolitis, croup, obstructive sleep apnea, diabetic ketoacidosis, seizure disorder /// concept: Croup
      432571, # source: Very High Risk Diagnoses: None, cardiac arrest preceding ICU admission, severe combined immune deficiency, leukemia or lymphoma after first induction, bone marrow transplant recipient, liver failure /// concept: Malignant lymphoma
      4029498, # source: Low Risk Diagnoses: None, asthma, bronchiolitis, croup, obstructive sleep apnea, diabetic ketoacidosis, seizure disorder /// concept: Seizure disorder
      317009, # source: Low Risk Diagnoses: None, asthma, bronchiolitis, croup, obstructive sleep apnea, diabetic ketoacidosis, seizure disorder /// concept: Asthma
      201957, # source: High Risk Diagnoses: (None, spontaneous cerbral hemorrhage, cardiomyopathy or myocarditis, hypoplastic left heart syndrome, neurodegenerative disorder, necrotizing enterocolitis) /// concept: Necrotizing enterocolitis in fetus OR newborn
      43530727, # source: High Risk Diagnoses: (None, spontaneous cerbral hemorrhage, cardiomyopathy or myocarditis, hypoplastic left heart syndrome, neurodegenerative disorder, necrotizing enterocolitis) /// concept: Spontaneous cerebral hemorrhage
      29783, # source: Very High Risk Diagnoses: None, cardiac arrest preceding ICU admission, severe combined immune deficiency, leukemia or lymphoma after first induction, bone marrow transplant recipient, liver failure /// concept: Severe combined immunodeficiency disease
      321042, # source: Very High Risk Diagnoses: None, cardiac arrest preceding ICU admission, severe combined immune deficiency, leukemia or lymphoma after first induction, bone marrow transplant recipient, liver failure /// concept: Cardiac arrest
      4165112, # source: Low Risk Diagnoses: None, asthma, bronchiolitis, croup, obstructive sleep apnea, diabetic ketoacidosis, seizure disorder /// concept: Bronchiolitis
      443727, # source: Low Risk Diagnoses: None, asthma, bronchiolitis, croup, obstructive sleep apnea, diabetic ketoacidosis, seizure disorder /// concept: Diabetic ketoacidosis
      314383, # source: High Risk Diagnoses: (None, spontaneous cerbral hemorrhage, cardiomyopathy or myocarditis, hypoplastic left heart syndrome, neurodegenerative disorder, necrotizing enterocolitis) /// concept: Myocarditis
      4245975, # source: Very High Risk Diagnoses: None, cardiac arrest preceding ICU admission, severe combined immune deficiency, leukemia or lymphoma after first induction, bone marrow transplant recipient, liver failure /// concept: Hepatic failure
      440207, # source: High Risk Diagnoses: (None, spontaneous cerbral hemorrhage, cardiomyopathy or myocarditis, hypoplastic left heart syndrome, neurodegenerative disorder, necrotizing enterocolitis) /// concept: Hypoplastic left heart syndrome
      42537745, # source: Very High Risk Diagnoses: None, cardiac arrest preceding ICU admission, severe combined immune deficiency, leukemia or lymphoma after first induction, bone marrow transplant recipient, liver failure /// concept: Bone marrow transplant present
      442588, # source: Low Risk Diagnoses: None, asthma, bronchiolitis, croup, obstructive sleep apnea, diabetic ketoacidosis, seizure disorder /// concept: Obstructive sleep apnea syndrome
      4213310, # source: High Risk Diagnoses: (None, spontaneous cerbral hemorrhage, cardiomyopathy or myocarditis, hypoplastic left heart syndrome, neurodegenerative disorder, necrotizing enterocolitis) /// concept: Degenerative disease of the central nervous system
      321319, # source: High Risk Diagnoses: (None, spontaneous cerbral hemorrhage, cardiomyopathy or myocarditis, hypoplastic left heart syndrome, neurodegenerative disorder, necrotizing enterocolitis) /// concept: Cardiomyopathy
      317510, # source: Very High Risk Diagnoses: None, cardiac arrest preceding ICU admission, severe combined immune deficiency, leukemia or lymphoma after first induction, bone marrow transplant recipient, liver failure /// concept: Leukemia
      5083, # source: Outpatient appointment type (telephone/ clinic visit etc.) /// concept: Telehealth
      42869590, # source: FiO2 /// concept: Oxygen/Gas total [Pure volume fraction] Inhaled gas
      3027946, # source: PaCO2 /// concept: Carbon dioxide [Partial pressure] in Arterial blood
      3004249 # source: Systolic blood pressure /// concept: Systolic blood pressure
    ),
    
    # 5.13 laboratory tests
    labTests = c(
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
    
    # 5.14 cardiac complications
    cardiacComplications = c(
      4051330,  # source: LCOS - renal support (CVVH/ PD) /// concept: Continuous venovenous hemofiltration
      4123933,  # source: ICU ward stay /// concept: Admission to pediatric intensive care unit
      4295705,  # source: Surgical injury - Reexplore for bleeding /// concept: Exploratory incision
      377091,  # source: Neurological injury - seizure /// concept: Seizure
      439847,  # source: Neurological injury - intracranial haemmhorage/ intracranial bleeding /// concept: Intracranial hemorrhage
      4308537,  # source: Surgical injury - Blood Loss /// concept: Injury to blood vessel during surgery
      321042,  # source: LCOS - Cardiac arrest /// concept: Cardiac arrest
      443454,  # source: Neurological injury - infarction /// concept: Cerebral infarction
      4185565,  # source: Low cardiac output state (LCOS) - ECMO /// concept: Low cardiac output syndrome
      42537043,  # source: CVL infections /// concept: CLABSI - central line associated bloodstream infection
      4306136  # source: Surgical injury - Chylothorax /// concept: Chylothorax
    ),

    # 5.15 Vital signs
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
    
    # 5.21 healthcare use
    healthcareUse = c(
      37174269,  # source: Diagnosis of acute infection having the potential for progression to sepsis1+2 /// concept: At increased risk of sepsis
      4032243,  # source: Dialysis /// concept: Dialysis procedure
      197320,  # source: acute kidney injury /// concept: Acute kidney injury
      4074689,  # source: "Chest opening, chest drainage" /// concept: Open drainage of pleural cavity
      4084670,  # source: Surgical admission (non-urgent) /// concept: Non-urgent surgical admission
      4070667,  # source: Urinary catheter /// concept: Urinary catheter
      44790095,  # source: Invasive ventilation /// concept: Invasive ventilation
      4052536,  # source: ECMO /// concept: Extracorporeal membrane oxygenation
      4051330,  # source: Dialysis /// concept: Continuous venovenous hemofiltration
      44790567,  # source: Origin (external) /// concept: Patient transfer from hospital to hospital
      133327,  # source: viremia /// concept: Viremia
      4097216,  # source: Endotracheal tube /// concept: Endotracheal tube
      4085730,  # source: Antibiotics /// concept: Antibiotic therapy
      4123946,  # source: Surgical admission (urgent) /// concept: Admission to surgical department
      4208341,  # source: organ transplant /// concept: Solid organ transplant
      4193843,  # source: Oxygenation index /// concept: Oxygenation index measurement
      4235043,  # source: Ventricular assist device (VAD) /// concept: Ventricular assist device
      443392,  # source: Cancer /// concept: Malignant neoplastic disease
      4181511,  # source: Antineoplastics /// concept: Administration of antineoplastic agent
      4294886,  # source: Origin (internal) /// concept: "Patient transfer, in-hospital"
      4301351,  # source: Surgery previous to prediction time point /// concept: Surgical procedure
      4140762,  # source: Antivirals /// concept: Antiviral therapy
      4177205,  # source: Peripheral IV cannulas /// concept: Cannulation
      4177224,  # source: Non-invasive ventilation /// concept: Non-invasive ventilation
      45758028,  # source: Arterial blood pressure catheter /// concept: Arterial blood pressure catheter
      44803020,  # source: Reason for admission /// concept: Primary reason for admission
      4179206,  # source: Central venous catheter /// concept: Central venous catheter
      132736,  # source: bacteremia /// concept: Bacteremia
      44783799,  # source: Tracheostomy /// concept: Exteriorization of trachea
      45768671,  # source: asplenia /// concept: Asplenia
      4324124,  # source: Peritoneal dialysis /// concept: Peritoneal dialysis
      435785,  # source: meningitis /// concept: Meningitis
      201820,  # source: Diagnosis of chronic condition /// concept: Diabetes mellitus
      42538045,  # source: Nasogastric/orogastric tube /// concept: Nasogastric/orogastric tube stylet
      37206601,  # source: ECMO type /// concept: Venoarterial extracorporeal membrane oxygenation
      37206603,  # source: ECMO type /// concept: Venovenous extracorporeal membrane oxygenation
      4314777,  # source: Immunosuppressors /// concept: Immunosuppressive therapy
      604243,  # source: neutropenia /// concept: Acquired neutropenia
      444187,  # source: injury with open wound /// concept: Open wound
      22281,  # source: sickle cell disease /// concept: Sickle cell-hemoglobin SS disease
      435613,  # source: cellulitis /// concept: Cellulitis
      4134120,  # source: cerebral palsy /// concept: Cerebral palsy
      444202,  # source: Abcess /// concept: Abscess
      312723,  # source: Diagnosis of chronic condition /// concept: Congenital heart disease
      255573,  # source: Diagnosis of chronic condition /// concept: Chronic obstructive lung disease
      255848,  # source: pneumonia /// concept: Pneumonia
      433740,  # source: immunodeficiency /// concept: Immunodeficiency disorder
      4080011,  # source: Diagnosis of organ system dysfunction /// concept: Organ dysfunction syndrome
      434821,  # source: Diagnosis of SIRS /// concept: Systemic inflammatory response syndrome
      81539,  # source: mitochondrial disease /// concept: Mitochondrial cytopathy
      4103588,  # source: tuberculosis /// concept: Acute tuberculosis
      443783,  # source: Diagnosis of chronic condition /// concept: Chronic disease
      4140977,  # source: Diagnosis of condition producing immunodeficiency /// concept: Secondary immune deficiency disorder
      201606,  # source: Crohn's disease /// concept: Crohn's disease
      4243475,  # source: hepatitis /// concept: Acute hepatitis
      4300243,  # source: surgical site inflammation /// concept: Postoperative complication
      439125,  # source: Down's syndrome /// concept: Complete trisomy 21 syndrome
      321042,  # source: cardiac arrest /// concept: Cardiac arrest
      4331815,  # source: UTI /// concept: Acute urinary tract infection
      195212,  # source: Cushing's disease /// concept: Hypercortisolism
      196152,  # source: peritonitis /// concept: Peritonitis
      314383,  # source: miocarditis /// concept: Myocarditis
      195314,  # source: nephrotic syndrome /// concept: Nephrotic syndrome
      441589,  # source: endocarditis /// concept: Endocarditis
      4271450,  # source: Diagnosis of acute infection having the potential for progression to sepsis1+2 /// concept: Acute infectious disease
      81893,  # source: ulcerative colitis /// concept: Ulcerative colitis
      80809,  # source: rheumatoid arthritis /// concept: Rheumatoid arthritis
      4322814,  # source: Meningo-encephalitis /// concept: Meningoencephalitis
      36716945,  # source: renal insufficiency /// concept: Renal insufficiency
      440448,  # source: appendicitis /// concept: Appendicitis
      433968,  # source: candidiasis /// concept: Candidiasis
      44807226,  # source: necrotizing enterocolitis /// concept: Necrotising enterocolitis
      132797,  # source: Previous diagnosis of sepsis measure /// concept: Sepsis
      21602722,  # source: Corticoids /// concept: CORTICOSTEROIDS FOR SYSTEMIC USE
      42869590,  # source: FiO2 /// concept: Oxygen/Gas total [Pure volume fraction] Inhaled gas
      42527086,  # source: Mean Airway Pressure (MAP) /// concept: Mean airway pressure
      1340204  # source: Previous diagnosis of sepsis measure /// concept: History of event
    ),
    
    # 5.22 vital signs UC2
    vitalSignsUC2 = c(
      4096101, # source: SpO2 /// concept: Measurement of oxygen saturation at periphery
      3025315, # source: Weight /// concept: Body weight
      3020891, # source: Body temperature /// concept: Body temperature
      3012888, # source: DBP (Diastolic blood pressure) /// concept: Diastolic blood pressure
      3004249, # source: SBP (Systolic Blood Pressure) /// concept: Systolic blood pressure
      3014315, # source: Urine output /// concept: Urine output
      3024171, # source: RR /// concept: Respiratory rate
      3027018 # source: HR /// concept: Heart rate
    ),
    
    # 5.23 physical examination signs
    physicalExaminationSigns = c(
      4224504, # source: Central pulse /// concept: Pulse
      4314539, # source: Peripheral pulse (pressure) /// concept: Arterial pulse pressure
      21490963, # source: Pupillary reactivity (right) /// concept: Right pupil Pupillary response
      3032652, # source: GCS /// concept: Glasgow coma scale
      3045676, # source: Capillary refill time /// concept: Capillary refill [Time]
      21491763, # source: Pupillary reactivity (left) /// concept: Left pupil Pupillary response
      3021415, # source: Pupillary size (left) /// concept: Left pupil Diameter Auto
      3027214 # source: Pupillary size (right) /// concept: Right pupil Diameter Auto
    ),
    
    # 5.24 lab tests UC2
    labTestsUC2 = c(
      37393605, # source: D-dimer /// concept: D-dimer level
      4017361, # source: Blood urea nitrogen /// concept: Blood urea nitrogen measurement
      4036356, # source: Bacterial pathogen detection /// concept: Detection of bacteria
      44789220, # source: Ionized calcium /// concept: Ionised calcium measurement
      4094436, # source: Fibrinogen /// concept: Fibrinogen measurement
      4118986, # source: Direct bilirubin /// concept: Bilirubin measurement
      4196268, # source: PCR panel /// concept: Polymerase chain reaction observation
      440029, # source: Viral pathogen detection /// concept: Viral disease
      4212899, # source: Leukocytes /// concept: Total white blood count
      4175016, # source: Thromboplastin time /// concept: Partial thromboplastin time, activated
      42536081, # source: MR-proADM /// concept: Adrenal medulla hormone
      4299649, # source: Quantification of colonies in culture /// concept: Quantitative microbial culture and measurement
      44806682, # source: Antibiotic resistance /// concept: Infection resistant to multiple antibiotics
      432545, # source: Name of bacterial pathogen detected /// concept: Bacterial infectious disease
      3013826, # source: Glucose /// concept: Glucose [Moles/volume] in Serum or Plasma
      3046279, # source: PCT /// concept: Procalcitonin [Mass/volume] in Serum or Plasma
      3033291 # source: Interleukin-6 /// concept: Interleukin 6 [Mass/volume] in Body fluid
    ),
    
    # 5.25 arterial blood gas
    arterialBloodGas = c(
      3003396, # source: Base excess /// concept: Base excess in Arterial blood by calculation
      3019977, # source: pH /// concept: pH of Arterial blood
      3008152, # source: HCO3 /// concept: Bicarbonate [Moles/volume] in Arterial blood
      3027946, # source: pCO2 /// concept: Carbon dioxide [Partial pressure] in Arterial blood
      3027801 # source: PaO2 /// concept: Oxygen [Partial pressure] in Arterial blood
    ),
    
    # 5.26 venous blood gas
    venousBloodGas = c(
      3009343, # source: pH /// concept: pH of Capillary blood
      3027273, # source: HCO3 /// concept: Bicarbonate [Moles/volume] in Venous blood
      3003129, # source: Base excess /// concept: Base excess in Capillary blood by calculation
      3002032, # source: Base excess /// concept: Base excess in Venous blood by calculation
      3015235, # source: HCO3 /// concept: Bicarbonate [Moles/volume] in Capillary blood
      3023024, # source: pCO2 /// concept: Carbon dioxide [Partial pressure] in Capillary blood
      3021447, # source: pCO2 /// concept: Carbon dioxide [Partial pressure] in Venous blood
      3028626, # source: PaO2 /// concept: Oxygen [Partial pressure] in Capillary blood
      3024354, # source: PaO2 /// concept: Oxygen [Partial pressure] in Venous blood
      3012544 # source: pH /// concept: pH of Venous blood
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
