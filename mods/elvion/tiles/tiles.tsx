<?xml version="1.0" encoding="UTF-8"?>
<tileset name="tiles" tilewidth="32" tileheight="32" tilecount="6" columns="10">
 <grid orientation="orthogonal" width="1" height="1"/>
 <tile id="0" type="floor">
  <properties>
   <property name="name" value="Пол"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
  <image width="32" height="32" source="floor.png"/>
 </tile>
 <tile id="1" type="floor">
  <properties>
   <property name="name" value="Трава"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
  <image width="32" height="32" source="grass.png"/>
 </tile>
 <tile id="2" type="wall">
  <properties>
   <property name="name" value="Стена"/>
   <property name="passable" type="bool" value="false"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
  <image width="32" height="32" source="wall.png"/>
 </tile>
 <tile id="3" type="floor">
  <properties>
   <property name="name" value="Лава"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
  <image width="32" height="32" source="lava.png"/>
 </tile>
 <tile id="4" type="floor">
  <properties>
   <property name="name" value="Вода"/>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
  <image width="32" height="32" source="water.png"/>
 </tile>
 <tile id="5" type="mountain">
  <properties>
   <property name="name" value="Гора"/>
   <property name="passable" type="bool" value="false"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
  <image width="32" height="32" source="mountain.png"/>
 </tile>
</tileset>
