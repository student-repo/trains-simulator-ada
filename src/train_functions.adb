with ada.Text_IO;
use ada.Text_IO;
package body train_functions is

   protected body Buffer is
          entry reserveRailway (railwayID: in Integer; ok: out Boolean) when Count < 100 is
      begin
         if railwaysAvailable2(railwayID) = true then
                      railwaysAvailable2(railwayID) := false;
                      ok := true;
                   else
                      ok := false;
                      end if;
      end reserveRailway;

                      entry reserveRailwaySwitch (railwaySwitchID: in Integer; ok: out Boolean) when Count < 100 is
                begin
         if railwaysSwitchesAvailable2(railwaySwitchID) = true then
                      railwaysSwitchesAvailable2(railwaySwitchID) := false;
                      ok := true;
                   else
                      ok := false;
                      end if;
                end reserveRailwaySwitch;

      entry reservePlatform (railwayID: in Integer; platformNumber: in Integer; platformID: out Integer) when Count < 100 is
         ok : Boolean := false;
      begin
         for i in 1 .. platformNumber loop
            if availablePlatforms(railwayID, i) = true then
               availablePlatforms(railwayID, i) := false;
               platformID := i;
               ok := true;
               exit;
            end if;
         end loop;
         if ok = false then
            platformID := -1;
            end if;
                end reservePlatform;

                entry releaseRailway (railwayID: in Integer) when true is
      begin
                   railwaysAvailable2(railwayID) := true;
                end releaseRailway;

                entry  releaseRailwaySwitch (railwaySwitchID: in Integer) when true is
      begin
                   railwaysSwitchesAvailable2(railwaySwitchID) := true;
                end releaseRailwaySwitch;

                entry  releasePlatform (railwayID: in Integer; platformID: in Integer) when true is
                begin
                   availablePlatforms(railwayID, platformID) := true;
                end releasePlatform;
      entry setReportEntry (singleEntry : in String; timeFromStart: in Integer; yearFromStart : Integer) when true is
         Output : File_Type;
         timeNow : Time;
         speedVar : Integer;
         helpVar : Integer;
      begin
         if yearFromStart /=0 then
            reportDate := Formatting.Time_Of(reportYear + yearFromStart, reportMonth, reportDay, Duration(Float(reportHour * 3600 + reportMinute * 60 + reportSecond)));
         end if;
         speedVar := timeFromStart;
         helpVar := timeFromStart / 100;
         timeNow := reportDate;
         --        this is hack - I can't add reportDate + timeDiff
         if helpVar > 0 then
            for i in 1 .. helpVar loop
               timeNow := timeNow + 100.0;
               speedVar := speedVar - 100;
            end loop;
            end if;

         for i in 1 .. speedVar loop
            timeNow := timeNow + 1.0;
            end loop;
         Open (File => Output,
           Mode => Append_File,
               Name => "report");
         Put_Line (Output, singleEntry & " " & Image(timeNow));
         Close (Output);
      end setReportEntry;
       end Buffer;


  procedure incrementRailway(t : in out main_types.train) is
   begin
      if t.currentRailwayIndex < t.routeLength then
         if t.route(t.currentRailwayIndex + 1).id = -1 then
            t.currentRailwayIndex := 0;
            else
           t.currentRailwayIndex := t.currentRailwayIndex + 1;
         end if;
      else
         t.currentRailwayIndex := 0;
         end if;
   end incrementRailway;



   function getCurrentSpeed(t : in main_types.train) return Integer is
   begin
      if t.maxSpeed < t.route(t.currentRailwayIndex).maxSpeed then
         return t.maxSpeed;
           end if;
      return t.route(t.currentRailwayIndex).maxSpeed;
   end;


      function getTimeToWait(t : in Integer) return Float is
   begin
      return (Float(My_Glob_Sec) * Float(t)) / 3600.0;
   end;


      function getNextRailwayIndex(t : in train) return Integer is
   begin
      if t.currentRailwayIndex < t.routeLength then
         if t.route(t.currentRailwayIndex + 1).id = -1 then
         return 0;
           end if;
         return t.currentRailwayIndex + 1;
      end if;
      return 0;
   end;




   function railwaySwitchEquals(s1, s2 : main_types.railwaySwitch) return Boolean is
   begin
      return s1.id = s2.id;
   end;


   function getRailwayTravelTime(t : in main_types.train) return Integer is
   begin
      return Integer(Float(t.route(t.currentRailwayIndex).length) / (Float(getCurrentSpeed(t)) * 1000.0 / 3600.0));
   end;

   function getPreviousRailwaySwitch(t : in main_types.train) return railwaySwitch is
   begin
      if t.currentRailwayIndex /= 0 then
         if railwaySwitchEquals(t.route(t.currentRailwayIndex).railwaySwitch1, t.route(t.currentRailwayIndex - 1).railwaySwitch2) or
         railwaySwitchEquals(t.route(t.currentRailwayIndex).railwaySwitch1, t.route(t.currentRailwayIndex - 1).railwaySwitch1) then
            return t.route(t.currentRailwayIndex).railwaySwitch1;
         end if;
         return t.route(t.currentRailwayIndex).railwaySwitch2;
      else
         if railwaySwitchEquals(t.route(t.currentRailwayIndex).railwaySwitch1, t.route(getLastRailwayIndex(t)).railwaySwitch2) or
         railwaySwitchEquals(t.route(t.currentRailwayIndex).railwaySwitch1, t.route(getLastRailwayIndex(t)).railwaySwitch1) then
            return t.route(t.currentRailwayIndex).railwaySwitch1;
         end if;
         return t.route(t.currentRailwayIndex).railwaySwitch2;
      end if;
   end;


   function getLastRailwayIndex(t : in train) return Integer is
      x : Integer;
   begin
      if t.route(t.routeLength).id /= -1 then
         return t.routeLength;
      else
         x := t.routeLength;
         while t.route(x).id < 0 loop
            x := x - 1;
         end loop;
         return x;
         end if;
   end;



      function getNextRailwaySwitch(t : in main_types.train) return railwaySwitch is
   begin
      if t.currentRailwayIndex <  t.routeLength - 1 then
         if railwaySwitchEquals(t.route(t.currentRailwayIndex).railwaySwitch1, t.route(t.currentRailwayIndex + 1).railwaySwitch2) or
         railwaySwitchEquals(t.route(t.currentRailwayIndex).railwaySwitch1, t.route(t.currentRailwayIndex + 1).railwaySwitch1) then
            return t.route(t.currentRailwayIndex).railwaySwitch1;
         end if;
         return t.route(t.currentRailwayIndex).railwaySwitch2;
      else
         if railwaySwitchEquals(t.route(t.currentRailwayIndex).railwaySwitch1, t.route(0).railwaySwitch2) or
         railwaySwitchEquals(t.route(t.currentRailwayIndex).railwaySwitch1, t.route(0).railwaySwitch1) then
            return t.route(t.currentRailwayIndex).railwaySwitch1;
         end if;
         return t.route(t.currentRailwayIndex).railwaySwitch2;
      end if;
   end;

   procedure printInfoCurrentRailway(t : in out main_types.train) is
   begin
                  if t.babblerMode then
               Put_Line(t.name & " is comming from the railwaySwith with id: " & Integer'Image(getPreviousRailwaySwitch(t).id) &
                          " to railwaySwitch with id: " & Integer'Image(getNextRailwaySwitch(t).id) & " Travel time: " & Integer'Image(getRailwayTravelTime(t)) & "(" & Float'Image(getTimeToWait(getRailwayTravelTime(t))) & ")");
         end if;
   end printInfoCurrentRailway;

   procedure simulateCurrentRailwayTime(t : in out main_types.train) is
   begin
      appendTimeFromStart(t, getRailwayTravelTime(t));
           delay Duration(getTimeToWait(getRailwayTravelTime(t)));
   end simulateCurrentRailwayTime;

     procedure appendTimeFromStart(t : in out main_types.train; tim: in Integer) is
   begin
      if t.timeFromStart > 31536000 then
         t.timeFromStart := t.timeFromStart mod 31536000;
         t.yearsFromStart := t.yearsFromStart + 1;
      end if;
      t.timeFromStart := t.timeFromStart + tim;
   end appendTimeFromStart;

   procedure handleUseRailwaySwitch(t : in out train; railwaSwitchesAvailable : in out Arr_Bool_Type) is
      railwaySwitchFree : Boolean := false;
   begin
      loop
         Buffer.reserveRailwaySwitch(getNextRailwaySwitch(t).id,railwaySwitchFree);
         if railwaySwitchFree then
                        if t.babblerMode then
               Put_Line("Train " & t.name & " is on railwaySwitch with id: " & Integer'Image(getNextRailwaySwitch(t).id) & ". We must wait " & Integer'Image(getNextRailwaySwitch(t).useTime) & " (" &
                          Float'Image(getTimeToWait(getNextRailwaySwitch(t).useTime)) & " )");
            end if;
            appendTimeFromStart(t, getNextRailwaySwitch(t).useTime);
                    delay Duration(getTimeToWait(getNextRailwaySwitch(t).useTime));
            exit;
         end if;
                     if t.babblerMode then
            Put_Line("Train " & t.name & " is before railwaySwitch with id: " & Integer'Image(getNextRailwaySwitch(t).id) & ". We must wait for permission to use it" );
         end if;
         appendTimeFromStart(t, Repeat_Request_Time);
        delay Duration(getTimeToWait(Repeat_Request_Time));
      end loop;
   end;


    procedure handleChangeNormalRailway(t : in out train; railwaysAvailable : in out Arr_Bool_Type; start : in Integer) is
      railwayIsFree : Boolean := false;
            railwayFree : Boolean := false;
   begin
      loop
         Buffer.reserveRailway(t.route(getNextRailwayIndex(t)).id - start ,railwayFree);
         if railwayFree then
            exit;
         end if;
                     if t.babblerMode then
            Put_Line("Train " & t.name & " is before changing railway with id: " & Integer'Image(t.route(getNextRailwayIndex(t)).id - start) & ". We have to wait until it will be free" );
         end if;
         appendTimeFromStart(t, Repeat_Request_Time);
        delay Duration(getTimeToWait(Repeat_Request_Time));
      end loop;
   end;


       procedure handleReleasingRailwaySwitch(t : in train; railwaSwitchesAvailable : in out Arr_Bool_Type) is
   begin
      Buffer.releaseRailwaySwitch(getNextRailwaySwitch(t).id);
                  if t.babblerMode then
         Put_Line(t.name & " left switch railway with id: " & Integer'Image(getNextRailwaySwitch(t).id));
         end if;
   end;

   procedure handleReleasingRailway(t : in train; railwaAvailable : in out Arr_Bool_Type) is
   begin
      Buffer.releaseRailway(t.route(t.currentRailwayIndex).id);
                  if t.babblerMode then
         Put_Line("Railway with id: " & Integer'Image(t.route(t.currentRailwayIndex).id) & " is now free");
         end if;
   end;


         procedure handleChangeStationRailway(t : in out train; platformsInfo : in out platformAvailable; start1 : in Integer) is
      platformID : Integer := -1;
   begin
      loop
         platformID := findFirstAvailablePlatform(t.route(getNextRailwayIndex(t) - start1).id, platformsInfo);

         if platformID /= -1 then
            t.route(getNextRailwayIndex(t) - start1).platform := platformID;
            if t.babblerMode then
               Put_Line("Train " & t.name & " has reserved platform with platformID: " & Integer'Image(platformID) & " on station with id:  " & Integer'Image(t.route(getNextRailwayIndex(t) - start1).id));
               end if;
            exit;
         end if;
                     if t.babblerMode then
            Put_Line("Train " & t.name & " have to wait for free platform, on station with id: " & Integer'Image(t.route(getNextRailwayIndex(t) - start1).id));
         end if;
         appendTimeFromStart(t, Repeat_Request_Time);
        delay Duration(getTimeToWait(Repeat_Request_Time));
      end loop;
   end;


       procedure handleChangeRailway(t : in out train; platformsInfo : in out platformAvailable; railwaysAvailable : in out Arr_Bool_Type; start : in Integer) is
   begin
      if t.route(getNextRailwayIndex(t)).station then
         handleChangeStationRailway(t, platformsInfo, 0);
      else
         handleChangeNormalRailway(t, railwaysAvailable, start);
         end if;

   end;



            procedure handleStationIfExist(t : in out train; platformsInfo : in out platformAvailable) is
   begin
      if t.route(t.currentRailwayIndex).station then
                       Buffer.setReportEntry("Train " & t.name & " arrived on station with id " & Integer'Image(t.route(t.currentRailwayIndex).id) & " on a platform" &
                                               Integer'Image(t.route(t.currentRailwayIndex).platform),  t.timeFromStart, t.yearsFromStart);
         if t.babblerMode then

         Put_Line("Train " & t.name &  " is staying on station with id: " & Integer'Image(t.route(t.currentRailwayIndex).id) & " on a platform" &
                    Integer'Image(t.route(t.currentRailwayIndex).platform) & " time: " & Integer'Image(platformsInfo(t.route(t.currentRailwayIndex).id + 1,
                    t.route(t.currentRailwayIndex).platform).downtime) & " (" & Float'Image(getTimeToWait(platformsInfo(t.route(t.currentRailwayIndex).id + 1,
                    t.route(t.currentRailwayIndex).platform).downtime)) & ")");
            end if;
         appendTimeFromStart(t, platformsInfo(t.route(t.currentRailwayIndex).id + 1, t.route(t.currentRailwayIndex).platform).downtime);
                 delay Duration(getTimeToWait(platformsInfo(t.route(t.currentRailwayIndex).id + 1, t.route(t.currentRailwayIndex).platform).downtime));
         end if;

   end;

       procedure handleReleasingStationt(t : in out train; platformsInfo : in out platformAvailable) is
   begin
      if t.route(t.currentRailwayIndex).station then

         Buffer.releasePlatform(t.route(t.currentRailwayIndex).id + 1, t.route(t.currentRailwayIndex).platform);

                     if t.babblerMode then
         Put_Line(t.name & " left station with id: " & Integer'Image(t.route(t.currentRailwayIndex).id) & " and platform id " & Integer'Image(t.route(t.currentRailwayIndex).platform));
            t.route(t.currentRailwayIndex).platform := -1;
            end if;
         end if;

   end;


   procedure taskBody(t : in out train; railwaysSwitchesAvailable : in out Arr_Bool_Type; railwaysAvailable : in out Arr_Bool_Type; platformsInfo : in out platformAvailable) is
   begin
                  if t.babblerMode then
         Ada.Text_IO.Put_Line(t.name & " start route");
         end if;
      handleChangeStationRailway(t, platformsInfo, 1);
      loop
         printInfoCurrentRailway(t);
         simulateCurrentRailwayTime(t);
         handleStationIfExist(t, platformsInfo);
         handleUseRailwaySwitch(t, railwaysSwitchesAvailable);
         handleChangeRailway(t, platformsInfo, railwaysAvailable, 0);
         handleReleasingStationt(t, platformsInfo);
         handleReleasingRailwaySwitch(t, railwaysSwitchesAvailable);
         handleReleasingRailway(t, railwaysAvailable);
         incrementRailway(t);
             end loop;
   end;




   function findFirstAvailablePlatform(railwayID : in Integer; platformsInfo : in out platformAvailable) return Integer is
      platformsNumber : Integer := 0;
      platformID : Integer := -1;
   begin
            for i in platformsInfo'Range(2) loop
         if platformsInfo(railwayID + 1, i).free then
            platformsNumber := platformsNumber + 1;
            end if;
      end loop;

      Buffer.reservePlatform(railwayID + 1,platformsNumber, platformID);
      return platformID;
   end;








 end train_functions;
