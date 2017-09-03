with main_types;
use main_types;
with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
package train_functions is

   My_Glob_Sec : Integer;
   Repeat_Request_Time : Integer := 5;
   reportDate : Time;
   reportYear : Integer;
   reportMonth : Integer;
   reportDay : Integer;
   reportHour : Integer;
   reportMinute: Integer;
   reportSecond : Integer;
   procedure appendTimeFromStart(t : in out main_types.train; tim: in Integer);
   function getTimeToWait(t : in Integer) return Float;
   procedure incrementRailway(t : in out train);
   function getCurrentSpeed(t : in train) return Integer;
   function getRailwayTravelTime(t : in train) return Integer;
   function railwaySwitchEquals(s1, s2 : railwaySwitch) return Boolean;
   function getPreviousRailwaySwitch(t : in train) return railwaySwitch;
   function getNextRailwaySwitch(t : in train) return railwaySwitch;
   procedure printInfoCurrentRailway(t : in out train);
   procedure simulateCurrentRailwayTime(t : in out train);
   procedure handleUseRailwaySwitch(t : in out train; railwaSwitchesAvailable : in out Arr_Bool_Type);
   procedure handleChangeNormalRailway(t : in out train; railwaysAvailable : in out Arr_Bool_Type; start : in Integer);
   procedure handleReleasingRailwaySwitch(t : in train; railwaSwitchesAvailable : in out Arr_Bool_Type);
   procedure handleReleasingRailway(t : in train; railwaAvailable : in out Arr_Bool_Type);
   procedure taskBody(t : in out train; railwaysSwitchesAvailable : in out Arr_Bool_Type; railwaysAvailable : in out Arr_Bool_Type; platformsInfo : in out platformAvailable);
   function findFirstAvailablePlatform(railwayID : in Integer; platformsInfo : in out platformAvailable) return Integer;
   procedure handleStationIfExist(t : in out train; platformsInfo : in out platformAvailable);
   procedure handleChangeRailway(t : in out train; platformsInfo : in out platformAvailable; railwaysAvailable : in out Arr_Bool_Type; start : in Integer);
   procedure handleReleasingStationt(t : in out train; platformsInfo : in out platformAvailable);
   function getLastRailwayIndex(t : in train) return Integer;

     protected  Buffer is
              entry reserveRailway (railwayID: in Integer; ok: out Boolean);
              entry reserveRailwaySwitch (railwaySwitchID: in Integer; ok: out Boolean);
              entry reservePlatform (railwayID: in Integer; platformNumber: in Integer; platformID: out Integer);
              entry releaseRailway (railwayID: in Integer);
              entry releaseRailwaySwitch (railwaySwitchID: in Integer);
      entry releasePlatform (railwayID: in Integer; platformID: in Integer);
              entry setReportEntry (singleEntry : in String; timeFromStart: in Integer; yearFromStart : Integer);
     private
        railwaysAvailable2 : Arr_Bool_Type(0 .. 50) := (others => true);
              railwaysSwitchesAvailable2 : Arr_Bool_Type(0 .. 50) := (others => true);
      In_P,Out_P, Count: Integer := 0;
      reportCounter : Integer := 1;
      availablePlatforms: platformAvBuffer(1 .. 50, 1 .. 50) := (others => (others => true));
     end Buffer;


end train_functions;
