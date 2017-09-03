with Ada.Directories, Ada.Direct_IO, Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with json_functions;
use json_functions;
with main_types;
use main_types;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with train_functions;
use train_functions;
with GNAT.OS_Lib;
with train_functions;
use train_functions;
with GNAT.Calendar.Time_IO;
with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;

procedure main is

     File_Name : String  := "config2.json";
   File_Size : Natural := Natural (Ada.Directories.Size (File_Name));

   Input, Output : File_Type;

   trainNumber : Integer := 0;
   railwaySwitchNumber : Integer := 0;
   railwayNumber : Integer := 0;
   maxPlatformsNumber : Integer := 0;
   maxRouteLenght : Integer := 0;
   fieldIndex : Integer := 0;
   temp_max_speed : Integer := 0;
   mode: Integer := 1;
   babblerMode : Boolean := false;
   selectTrainNumber : Integer := 0;
   trainDisplayOption : Integer := 1;







     subtype File_String    is String (1 .. File_Size);
     package File_String_IO is new Ada.Direct_IO (File_String);

     File     : File_String_IO.File_Type;
   Contents : File_String;





begin
   Create (File => Output,
           Mode => Out_File,
           Name => "report");

   Close (Output);

     File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                                 Name => File_Name);
     File_String_IO.Read  (File, Item => Contents);
     File_String_IO.Close (File);

--     Put (Contents);
--     Put_Line(" ");
--     Put_Line(" ");

   reportYear := getJSONFieldIntValue(Contents, """report_year"": "  );
   reportMonth := getJSONFieldIntValue(Contents, """report_month"": "  );
   reportHour := getJSONFieldIntValue(Contents, """report_hour"": "  );
   reportMinute := getJSONFieldIntValue(Contents, """report_minutes"": "  );
   reportSecond := getJSONFieldIntValue(Contents, """report_seconds"": "  );
   reportDay := getJSONFieldIntValue(Contents, """report_day"": "  );

     trainNumber := getJSONFieldIntValue(Contents, """trains_number"": "  );
     railwaySwitchNumber := getJSONFieldIntValue(Contents, """railway_switch_number"": "  );
     railwayNumber := getJSONFieldIntValue(Contents, """railways_number"": "  );
     maxPlatformsNumber := getJSONFieldIntValue(Contents, """max_platforms_number"": "  );
     maxRouteLenght := getJSONFieldIntValue(Contents, """max_route_lenght"": "  );

   declare
        type trainArr is array (Integer range <>) of train(5 + Integer'Image(trainNumber)'Length, maxRouteLenght);
        railwaySwitches : railwaySwitchArr(0 .. railwaySwitchNumber);
        railways : railwayArr(0 .. railwayNumber);
        platforms : IntegerArr(0 .. maxPlatformsNumber);
        trainRouteIndexes : IntegerArr(0 .. maxRouteLenght);
        trains : trainArr(0 .. trainNumber - 1);
      trainRailways : railwayArr(0 .. maxRouteLenght);
      railwaysAvailable : Arr_Bool_Type(0 .. railwayNumber) := (others => true);
      railwaysSwitchesAvailable : Arr_Bool_Type(0 .. railwaySwitchNumber) := (others => true);
      availablePlatforms: platformAvailable(1 .. railwayNumber + 1, 1 .. maxPlatformsNumber + 1);

   begin

      reportDate := Formatting.Time_Of(reportYear, reportMonth, reportDay, Duration(Float(reportHour * 3600 + reportMinute * 60 + reportSecond)));

      setJSONRailwaySwitches(Contents, """railway_switches"": ", """use_time"": ", """id"": ", railwaySwitches, railwaySwitchNumber);
      setJSONRailways(Contents, """railways"": ", """max_speed"": ", """length"": ", """railway_switch_1_ID"": ", """railway_switch_2_ID"": ", """platforms"": ",
                            """id"": ", railways,railwaySwitches, availablePlatforms, maxPlatformsNumber + 1, railwayNumber);
      setJSONArray(Contents, """platformsss"": ",platforms, maxPlatformsNumber);

      Put_Line("Please determine thow much secounds is one hour in simulation: ");
           My_Glob_Sec := Integer'Value(Get_Line);
      Put_Line("Please select mode: ");
   Put_Line("1. Babbler mode ");
   Put_Line("2. Still mode ");
      mode := Integer'Value(Get_Line);
      if mode = 1 then
         babblerMode := true;
      end if;




        fieldIndex := Index (Contents, """trains"": ");
        for i in 0 .. trainNumber - 1 loop
           temp_max_speed := getJSONFieldIntValue(Contents(fieldIndex .. Contents'Last), """max_speed"": "  );

           setJSONArray(Contents(fieldIndex .. Contents'Last),  """route"": ",trainRouteIndexes, maxRouteLenght );

            for j in trainRouteIndexes'Range loop
               if trainRouteIndexes(j) /= -1 then
                  trainRailways(j) := railways(trainRouteIndexes(j));
               else
                  trainRailways(j) := (-1, -1, (-1, -1), (-1, -1), false,  -1, -1);
                  end if;
         end loop;
             trains(i) := (5 + Integer'Image(trainNumber)'Length, maxRouteLenght, "train" & Integer'Image(i),  trainRailways, temp_max_speed, 0, babblerMode, 0 ,0);
           fieldIndex := Index (Contents(fieldIndex .. Contents'Last), "}");
           fieldIndex := fieldIndex + 1;
      end loop;

      declare



         task type T (t : Integer);
         type T_P is access T;
          type My_Arr is array (Integer range <>) of T_P;
         task body T is
         begin
             taskBody(trains(t), railwaysSwitchesAvailable, railwaysAvailable, availablePlatforms);
         end T;

         Arr : My_Arr(0 .. trainNumber - 1);


      begin

         --                   delay Duration(1.4);
--------------HANDLE STILL MODE----------------------------------------------

         if babblerMode = false then
            for j in 0 .. trainNumber - 1 loop
                  Arr(j) := new T (t => j);
               end loop;
            loop
               Put_Line("Please select train: ");
                 for i in 0 .. trainNumber - 1 loop
                    Put_Line(Integer'Image(i) & ". " & trains(i).name);
               end loop;
               selectTrainNumber := Integer'Value(Get_Line);

          Put_Line("Please select: ");
         Put_Line("1. Get current railway id ");
         Put_Line("2. Get current speed");
               trainDisplayOption := Integer'Value(Get_Line);
               Put_Line(" ");

               if trainDisplayOption = 1 then
                  Put_Line(trains(selectTrainNumber).name & " current railwayID: " & Integer'Image(trains(selectTrainNumber).route(trains(selectTrainNumber).currentRailwayIndex).id));
               else
                  Put_Line(trains(selectTrainNumber).name & " current speed: " & Integer'Image(getCurrentSpeed(trains(selectTrainNumber))));
               end if;
                                 Put_Line(" ");
               end loop;
         end if;

                for j in 0 .. trainNumber - 1 loop
                  Arr(j) := new T (t => j);
               end loop;


      end;
      end;

end main;



