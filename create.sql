CREATE TABLE uw_Service_user(uw_id int8 NOT NULL, uw_name text NOT NULL,
 PRIMARY KEY (uw_iD)
  
 );
 
 CREATE TABLE uw_Service_project(uw_id int8 NOT NULL, uw_name text NOT NULL, 
                                  uw_description text NOT NULL, 
                                  uw_visible bool NOT NULL, 
                                  uw_user_id int8 NOT NULL,
  PRIMARY KEY (uw_iD),
   CONSTRAINT uw_Service_project_USER_ID
    FOREIGN KEY (uw_uSER_ID) REFERENCES uw_Service_user (uw_iD)
  );
  
  CREATE TABLE uw_Service_taskTable(uw_id int8 NOT NULL, uw_name text NOT NULL, 
                                     uw_description text NOT NULL, 
                                     uw_user_id int8 NOT NULL,
   PRIMARY KEY (uw_iD),
    CONSTRAINT uw_Service_taskTable_USER_ID
     FOREIGN KEY (uw_uSER_ID) REFERENCES uw_Service_user (uw_iD)
   );
   
   CREATE TABLE uw_Service_projectTask(uw_project_id int8 NOT NULL, 
                                        uw_task_id int8 NOT NULL, 
                                        uw_visible bool NOT NULL,
    PRIMARY KEY (uw_tASK_ID, uw_pROJECT_ID),
     CONSTRAINT uw_Service_projectTask_PROJECT_ID
      FOREIGN KEY (uw_pROJECT_ID) REFERENCES uw_Service_project (uw_iD),
                                                                        
      CONSTRAINT uw_Service_projectTask_TASK_ID
       FOREIGN KEY (uw_tASK_ID) REFERENCES uw_Service_taskTable (uw_iD)
    );
    
    CREATE TABLE uw_Service_entry(uw_project_id int8 NOT NULL, 
                                   uw_task_id int8 NOT NULL, 
                                   uw_date timestamp NOT NULL, 
                                   uw_time float8 NOT NULL,
     PRIMARY KEY (uw_dATE, uw_pROJECT_ID, uw_tASK_ID),
      CONSTRAINT uw_Service_entry_PROJECT_ID
       FOREIGN KEY (uw_pROJECT_ID) REFERENCES uw_Service_project (uw_iD),
                                                                         
       CONSTRAINT uw_Service_entry_TASK_ID
        FOREIGN KEY (uw_tASK_ID) REFERENCES uw_Service_taskTable (uw_iD)
     );
     
     