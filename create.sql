CREATE TABLE uw_Timesheet_user_table(uw_id int8 NOT NULL, uw_name text NOT NULL,
 PRIMARY KEY (uw_iD)
  
 );
 
 CREATE TABLE uw_Timesheet_project_table(uw_id int8 NOT NULL, 
                                          uw_name text NOT NULL, 
                                          uw_description text NOT NULL, 
                                          uw_user_id int8 NOT NULL,
  PRIMARY KEY (uw_iD),
   CONSTRAINT uw_Timesheet_project_table_USER_ID
    FOREIGN KEY (uw_uSER_ID) REFERENCES uw_Timesheet_user_table (uw_iD)
  );
  
  CREATE TABLE uw_Timesheet_task_table(uw_id int8 NOT NULL, 
                                        uw_name text NOT NULL, 
                                        uw_description text NOT NULL, 
                                        uw_user_id int8 NOT NULL,
   PRIMARY KEY (uw_iD),
    CONSTRAINT uw_Timesheet_task_table_USER_ID
     FOREIGN KEY (uw_uSER_ID) REFERENCES uw_Timesheet_user_table (uw_iD)
   );
   
   CREATE TABLE uw_Timesheet_project_task_table(uw_project_id int8 NOT NULL, 
                                                 uw_task_id int8 NOT NULL,
    PRIMARY KEY (uw_tASK_ID, uw_pROJECT_ID),
     CONSTRAINT uw_Timesheet_project_task_table_PROJECT_ID
      FOREIGN KEY (uw_pROJECT_ID) REFERENCES uw_Timesheet_project_table (uw_iD),
                                                                                
      CONSTRAINT uw_Timesheet_project_task_table_TASK_ID
       FOREIGN KEY (uw_tASK_ID) REFERENCES uw_Timesheet_task_table (uw_iD)
    );
    
    CREATE TABLE uw_Timesheet_entry_table(uw_project_id int8 NOT NULL, 
                                           uw_task_id int8 NOT NULL, 
                                           uw_date timestamp NOT NULL, 
                                           uw_time int8 NOT NULL,
     PRIMARY KEY (uw_dATE, uw_pROJECT_ID, uw_tASK_ID),
      CONSTRAINT uw_Timesheet_entry_table_PROJECT_ID
       FOREIGN KEY (uw_pROJECT_ID) REFERENCES uw_Timesheet_project_table (uw_iD),
                                                                                 
       CONSTRAINT uw_Timesheet_entry_table_TASK_ID
        FOREIGN KEY (uw_tASK_ID) REFERENCES uw_Timesheet_task_table (uw_iD)
     );
     
     