;==============================================================================
;
;	User Configuration Settings
;

; Write directory
PAT_NO = '037'

;MAIN_DIR = "C:\documents and settings\mkusano\work\dynamic pet\data\"
 MAIN_DIR = 'C:\MS\Phase3_ROI\07_Dynamic_PET_CT - 25 Aug 2009, Register Pre on Intra\'
;MAIN_DIR = "C:\MS\Phase1_Acquisition\PVC\"

 WRITE_DIR = main_dir + '_Raw Patient Data\_Pre2Intra\'

 READ_DIR = main_dir + '_Raw Patient Data\S-' + PAT_NO + '\'
;READ_DIR = main_dir + "_Test_Raw Patient Data\S-"+PAT_NO+"\"

TEMP_DIR = write_dir
CT0_DIR		= "s0\"
PET0_DIR	= "s0\"
RS0_DIR		= "s0\"
CT0_FILTER	= "CT*.syn"
PET0_FILTER	= "*ctac_zm.img"
RS0_FILTER	= "RS*.dcm"
CT1_DIR		= "s1\"
PET1_DIR	= "s1\"
RS1_DIR		= "s1\
CT1_FILTER	= CT0_FILTER
PET1_FILTER	= PET0_FILTER
RS1_FILTER	= "RS*.dcm"

; File names
patName = "S"+PAT_NO
prePrefix = patName + '_pre'
postPrefix = patName + '_intra'

; Write images? (for debugging purposes)
bWriteImages = 1b
bDisplay = 0b