source("config.R")
source("utility_fun.R")

site <-  load_instrument("abcd_lt01",abcd_files_path)

####### fixes in 4.0

site$site_id_l[site$src_subject_id == "NDAR_INV2HYAENE6" & site$eventname == "2_year_follow_up_y_arm_1"] = "site08"
site$site_id_l[site$src_subject_id == "NDAR_INV4DVGGJE9" & site$eventname == "baseline_year_1_arm_1"] = "site22"
site$site_id_l[site$src_subject_id == "NDAR_INV6JF8WUYT" & site$eventname == "baseline_year_1_arm_1"] = "site21"
site$site_id_l[site$src_subject_id == "NDAR_INV6WV9X2KM" & site$eventname == "baseline_year_1_arm_1"] = "site22"
# site$site_id_l[site$src_subject_id == "NDAR_INV6WV9X2KM" & site$eventname == "1_year_follow_up_y_arm_1"] = "site21"
# site$site_id_l[site$src_subject_id == "NDAR_INV6WV9X2KM" & site$eventname == "2_year_follow_up_y_arm_1"] = "site21"
site$site_id_l[site$src_subject_id == "NDAR_INVC7P1CVEU" & site$eventname == "baseline_year_1_arm_1"] = "site17"
site$site_id_l[site$src_subject_id == "NDAR_INVG19M2F39" & site$eventname == "baseline_year_1_arm_1"] = "site06"

site$site_id_l[site$src_subject_id == "NDAR_INVGFCRX7YW" & site$eventname == "baseline_year_1_arm_1"] = "site17"
site$site_id_l[site$src_subject_id == "NDAR_INVGVPPRTDN" & site$eventname == "baseline_year_1_arm_1"] = "site20"
site$site_id_l[site$src_subject_id == "NDAR_INVHAP0JZTR" & site$eventname == "baseline_year_1_arm_1"] = "site05"
site$site_id_l[site$src_subject_id == "NDAR_INVJHCBZTEX" & site$eventname == "baseline_year_1_arm_1"] = "site13"
# site$site_id_l[site$src_subject_id == "NDAR_INVJHCBZTEX" & site$eventname == "1_year_follow_up_y_arm_1"] = "site13"
# site$site_id_l[site$src_subject_id == "NDAR_INVJHCBZTEX" & site$eventname == "2_year_follow_up_y_arm_1"] = "site13"
site$site_id_l[site$src_subject_id == "NDAR_INVLVLHRL2N" & site$eventname == "baseline_year_1_arm_1"] = "site21"
site$site_id_l[site$src_subject_id == "NDAR_INVNTAR3TAF" & site$eventname == "baseline_year_1_arm_1"] = "site17"
# site$site_id_l[site$src_subject_id == "NDAR_INVNTAR3TAF" & site$eventname == "1_year_follow_up_y_arm_1"] = "site17"
site$site_id_l[site$src_subject_id == "NDAR_INVR0TYK5V9" & site$eventname == "baseline_year_1_arm_1"] = "site22"
site$site_id_l[site$src_subject_id == "NDAR_INVRY96FYZ8" & site$eventname == "baseline_year_1_arm_1"] = "site05"
# site$site_id_l[site$src_subject_id == "NDAR_INVRY96FYZ8" & site$eventname == "1_year_follow_up_y_arm_1"] = "site05"
site$site_id_l[site$src_subject_id == "NDAR_INVT1C2GBHB" & site$eventname == "baseline_year_1_arm_1"] = "site19"
# site$site_id_l[site$src_subject_id == "NDAR_INVT1C2GBHB" & site$eventname == "1_year_follow_up_y_arm_1"] = "site19"
# site$site_id_l[site$src_subject_id == "NDAR_INVT1C2GBHB" & site$eventname == "2_year_follow_up_y_arm_1"] = "site19"
site$site_id_l[site$src_subject_id == "NDAR_INVUB6JP787" & site$eventname == "baseline_year_1_arm_1"] = "site13"
site$site_id_l[site$src_subject_id == "NDAR_INVUB6JP787" & site$eventname == "1_year_follow_up_y_arm_1"] = "site13"
site$site_id_l[site$src_subject_id == "NDAR_INVUFF64VGJ" & site$eventname == "baseline_year_1_arm_1"] = "site16"
# site$site_id_l[site$src_subject_id == "NDAR_INVUFF64VGJ" & site$eventname == "1_year_follow_up_y_arm_1"] = "site16"
site$site_id_l[site$src_subject_id == "NDAR_INVUKPZU1JW" & site$eventname == "baseline_year_1_arm_1"] = "site13"
# site$site_id_l[site$src_subject_id == "NDAR_INVUKPZU1JW" & site$eventname == "1_year_follow_up_y_arm_1"] = "site13"
site$site_id_l[site$src_subject_id == "NDAR_INVVT14CE3D" & site$eventname == "baseline_year_1_arm_1"] = "site22"
site$site_id_l[site$src_subject_id == "NDAR_INVWF7C1DEL" & site$eventname == "baseline_year_1_arm_1"] = "site09"
site$site_id_l[site$src_subject_id == "NDAR_INVXLFHB010" & site$eventname == "baseline_year_1_arm_1"] = "site16"
site$site_id_l[site$src_subject_id == "NDAR_INVY92TEZW6" & site$eventname == "baseline_year_1_arm_1"] = "site13"
# site$site_id_l[site$src_subject_id == "NDAR_INVY92TEZW6" & site$eventname == "1_year_follow_up_y_arm_1"] = "site13"


site$site_id_l_br = sub("site","",site$site_id_l)
site[,c("sched_delay", "site_id_l")] = NULL

write.csv(file = "outputs/site.csv",x = site, row.names = F, na = "")

