<?xml version="1.0" encoding="UTF-8"?>
<tileset name="objects" tilewidth="32" tileheight="32" tilecount="14" columns="0">
 <grid orientation="orthogonal" width="1" height="1"/>
 <tile id="0" type="player_start">
  <image width="32" height="32" source="player_start.png"/>
   <properties>
   <property name="name" value="Игрок"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
 </tile>
 <tile id="1" type="player_finish">
  <image width="32" height="32" source="finish.png"/>
   <properties>
   <property name="name" value="Выход"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
 </tile>
 <tile id="2" type="portal">
  <image width="32" height="32" source="portal.png"/>
   <properties>
   <property name="name" value="Портал"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
 </tile>
 <tile id="3" type="up_stairs">
  <image width="32" height="32" source="up.png"/>
   <properties>
   <property name="name" value="Лестница вверх"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
 </tile>
 <tile id="4" type="down_stairs">
  <image width="32" height="32" source="down.png"/>
   <properties>
   <property name="name" value="Лестница вниз"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
 </tile>
 <tile id="5" type="tree">
  <image width="32" height="32" source="oak.png"/>
  <properties>
   <property name="name" value="Дуб"/>
   <property name="passable" type="bool" value="false"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
 </tile>
 <tile id="6" type="hidden_door">
  <image width="32" height="32" source="../tiles/wall.png"/>
  <properties>
   <property name="name" value="Стена"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
 </tile>
 <tile id="7" type="closed_door">
  <image width="32" height="32" source="closed_door.png"/>
  <properties>
   <property name="name" value="Закрытая дверь"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
 </tile>
 <tile id="8" type="opened_door">
  <image width="32" height="32" source="opened_door.png"/>
   <properties>
   <property name="name" value="Открытая дверь"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
</tile>
 <tile id="9" type="trapped_chest">
  <image width="32" height="32" source="closed_chest.png"/>
  <properties>
   <property name="name" value="Закрытый сундук/Ловушкa"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
 </tile>
 <tile id="10" type="closed_chest">
  <image width="32" height="32" source="closed_chest.png"/>
  <properties>
   <property name="name" value="Закрытый сундук"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
 </tile>
 <tile id="11" type="opened_chest">
  <image width="32" height="32" source="opened_chest.png"/>
  <properties>
   <property name="name" value="Открытый сундук"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
 </tile>
 <tile id="12" type="mountain">
  <image width="32" height="32" source="mountain.png"/>
  <properties>
   <property name="name" value="Гора"/>
   <property name="passable" type="bool" value="false"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
 </tile>
 <tile id="13" type="tree">
  <image width="32" height="32" source="pine.png"/>
  <properties>
   <property name="name" value="Ель"/>
   <property name="passable" type="bool" value="false"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
 </tile>
</tileset>
