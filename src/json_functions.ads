with main_types; use main_types;

package json_functions is
   function getIntegerLength(s : in String) return Integer;
   function getJSONFieldIntValue(s : in String; field : in String) return Integer;
   procedure setJSONRailwaySwitches(s : in String; field : in String; use_time : in String; id : in String; railwaysSwitches : in out railwaySwitchArr; iter : in Integer);
   procedure setJSONRailways(s : in String; field : in String; max_speed : in String; lenght : in String;railwaySwitch1 : in String;
                             railwaySwitch2 : in String; platform : in String; id : in String;railways : in out railwayArr; railwaysSwitches : in railwaySwitchArr;
                             availablePlatforms :in out platformAvailable; maxPlatformsNumber : in Integer; iter : in Integer);
procedure setJSONArray(s : in String; field : in String; intArr : in out IntegerArr; max_platform_number : in Integer);
   end json_functions;
