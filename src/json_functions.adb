with Ada.Text_Io; use Ada.Text_Io;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with main_types; use main_types;
with main_types; use main_types;


package body json_functions is
   function getIntegerLength(s : in String) return Integer is
      firstDigitNumber : Integer := 0;
   begin
      for i in s'Range loop
         if Is_Digit(s(i)) then
            firstDigitNumber := firstDigitNumber + 1;
         else
           exit;

            end if;
      end loop;
      return firstDigitNumber;
        end;

   function getJSONFieldIntValue(s : in String; field : in String ) return Integer is
      fieldIndex : Integer := 0;
      value : Integer := 0;
      intLenght : Integer := 0;
      fieldValueIndex : Integer := 0;
   begin
      fieldIndex := Index (s, field);
      fieldValueIndex := fieldIndex + field'Length;
      intLenght := getIntegerLength(s(fieldValueIndex .. s'Last));
      value := Integer'Value(s(fieldValueIndex .. fieldValueIndex + intLenght - 1));

      return value;
   end;

   --      getJSONRailwaySwitches


   procedure setJSONRailwaySwitches(s : in String; field : in String; use_time : in String; id : in String; railwaysSwitches : in out railwaySwitchArr; iter : in Integer) is
      fieldIndex : Integer := 0;
      temp_use_time : Integer := 0;
            temp_id : Integer := 0;
   begin
      fieldIndex := Index (s, field);
      for i in 0 .. iter loop
         temp_use_time := getJSONFieldIntValue(s(fieldIndex .. s'Last), use_time  );
         temp_id := getJSONFieldIntValue(s(fieldIndex .. s'Last), id  );
         railwaysSwitches(i) := (temp_use_time, temp_id);
         fieldIndex := Index (s(fieldIndex .. s'Last), "}");
         fieldIndex := fieldIndex + 1;
         end loop;
   end;

      procedure setJSONArray(s : in String; field : in String; intArr : in out IntegerArr; max_platform_number : in Integer) is
      fieldIndex : Integer := 0;
      integer_lenght : Integer := 0;
      output_arr_iterator : Integer := 0;
   begin
      fieldIndex := Index (s, field);
      while s(fieldIndex) /= ']' loop
         integer_lenght := getIntegerLength(s(fieldIndex .. s'Last));
         if integer_lenght = 0 then
            fieldIndex := fieldIndex + 1;
         else
            intArr(output_arr_iterator) := Integer'Value(s(fieldIndex .. fieldIndex + integer_lenght - 1));
         output_arr_iterator := output_arr_iterator + 1;
         fieldIndex := fieldIndex + integer_lenght;
            end if;
      end loop;
      while output_arr_iterator <= max_platform_number loop
         intArr(output_arr_iterator) := -1;
         output_arr_iterator := output_arr_iterator + 1;
         end loop;
   end;

   procedure setJSONRailways(s : in String; field : in String; max_speed : in String; lenght : in String;railwaySwitch1 : in String;
                             railwaySwitch2 : in String; platform : in String; id : in String;railways : in out railwayArr; railwaysSwitches : in railwaySwitchArr;
                             availablePlatforms :in out platformAvailable; maxPlatformsNumber : in Integer; iter : in Integer) is
      fieldIndex : Integer := 0;
      temp_max_speed : Integer := 0;
      temp_lenght : Integer := 0;
      temp_railwaySwitch1 : Integer := 0;
      temp_railwaySwitch2 : Integer := 0;
      temp_station : Boolean := false;
      temp_id : Integer := 0;
      temp_platform : Integer := -1;

      platforms : IntegerArr(0 .. maxPlatformsNumber - 1);
   begin
      fieldIndex := Index (s, field);
      for i in 0 .. iter loop
         temp_max_speed := getJSONFieldIntValue(s(fieldIndex .. s'Last), max_speed  );
         temp_lenght := getJSONFieldIntValue(s(fieldIndex .. s'Last), lenght  );
         temp_railwaySwitch1 := getJSONFieldIntValue(s(fieldIndex .. s'Last), railwaySwitch1  );
         temp_railwaySwitch2 := getJSONFieldIntValue(s(fieldIndex .. s'Last), railwaySwitch2  );
         temp_station := false;
         temp_platform := -1;
            temp_id := getJSONFieldIntValue(s(fieldIndex .. s'Last), id  );

         setJSONArray(s(fieldIndex .. s'Last), platform ,platforms, maxPlatformsNumber -1 );

         for j in platforms'Range loop
                if platforms(j) /= -1 then
                   availablePlatforms(i + 1, j + 1) := (true, platforms(j), j);
                else
                   availablePlatforms(i + 1, j + 1) := (false, -1, -1);
                   end if;
         end loop;

         if platforms(0) /= -1 then
            temp_station := true;
            end if;
         railways(i) := (temp_max_speed, temp_lenght, railwaysSwitches(temp_railwaySwitch1), railwaysSwitches(temp_railwaySwitch2), temp_station, temp_platform, temp_id);
         fieldIndex := Index (s(fieldIndex .. s'Last), "}");
         fieldIndex := fieldIndex + 1;
         end loop;
   end;


      end json_functions;
