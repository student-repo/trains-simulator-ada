package main_types is

      type railwaySwitch is
   record
      useTime : Integer;
      id : Integer;
      end record;

     type railway is
   record
         maxSpeed : Integer;
         length : Integer;
         railwaySwitch1 : railwaySwitch;
         railwaySwitch2 : railwaySwitch;
         station : Boolean;
         platform : Integer;
         id : Integer;
      end record;

    type platform is
      record
         free : Boolean;
         downtime : Integer;
         id : Integer;
      end record;

   type platformArr is array (Integer range <>) of platform;

    type platformAvailable is
     array (Positive range <>, Positive range <>) of platform;

       type platformAvBuffer is
     array (Positive range <>, Positive range <>) of Boolean;

   type railwayArr is array (Integer range <>) of railway;
      type IntegerArr is array (Integer range <>) of Integer;

   type railwaySwitchArr is array (Integer range <>) of railwaySwitch;

   type train(nameLength : Natural; routeLength : Natural) is
      record
         name : String(1 .. nameLength);
         route : railwayArr(0 .. routeLength);
         maxSpeed : Integer;
         currentRailwayIndex : Integer;
         babblerMode: Boolean;
         timeFromStart: Integer;
         yearsFromStart: Integer;
      end record;

   type Arr_Bool_Type is array (Integer range <>) of Boolean;





end main_types;
