CREATE TABLE uw_Timesheet_user_table(uw_id int8 NOT NULL, uw_name text NOT NULL,
 PRIMARY KEY (uw_iD)
  
 );
 
 CREATE TABLE uw_Timesheet_project_table(uw_id int8 NOT NULL, 
                                          uw_name text NOT NULL, 
                                          uw_description text NOT NULL, 
                                          uw_visible bool NOT NULL, 
                                          uw_user_id int8 NOT NULL,
  PRIMARY KEY (uw_iD),
   CONSTRAINT uw_Timesheet_project_table_USER_ID
    FOREIGN KEY (uw_uSER_ID) REFERENCES uw_Timesheet_user_table (uw_iD)
  );
  
  