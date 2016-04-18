WECACquery <- function(
                       select = "*" ,
                       where = "(time_of_follow_up IN ('0','2')) AND (diagnose_at_baseline_blood_sampling = '1')"
                       ) {

    dbStatement = paste(
        "SELECT" , select ,
        "FROM PATIENT_INFO P
       LEFT JOIN LAB_BLOOD_TOTAL B ON P.FORUS_ID=B.FORUS_ID
       LEFT JOIN hs_medikament_skjema M ON P.FORUS_ID=M.FORUS_ID
       LEFT JOIN hs_hendelser_CVDNOR_mars14 C ON P.NORCAD_ID=C.NORCAD_ID
       LEFT JOIN hs_pasientrapport_skjema R ON P.FORUS_ID=R.FORUS_ID" ,
       "WHERE" , where , ";")

    if ( !exists("con") )
        con <- DBI::dbConnect(
        drv = RMySQL::MySQL() ,
        dbname = "norcad" )

    return(
        DBI::dbGetQuery(
            conn = con , 
            statement = dbStatement) )
}
