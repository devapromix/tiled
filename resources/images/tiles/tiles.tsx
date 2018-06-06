<?xml version="1.0" encoding="UTF-8"?>
<tileset name="tiles" tilewidth="32" tileheight="32" tilecount="3" columns="0">
 <grid orientation="orthogonal" width="1" height="1"/>
 <tile id="0" type="floor">
  <properties>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
  <image width="32" height="32" source="floor.png"/>
 </tile>
 <tile id="1" type="floor">
  <properties>
   <property name="passable" type="bool" value="true"/>
   <property name="transparent" type="bool" value="true"/>
  </properties>
  <image width="32" height="32" source="grass.png"/>
 </tile>
 <tile id="2" type="wall">
  <properties>
   <property name="passable" type="bool" value="false"/>
   <property name="transparent" type="bool" value="false"/>
  </properties>
  <image width="32" height="32" source="wall.png"/>
 </tile>
</tileset>
