agent(3, 3).
neighbor(2, 2).
neighbor(5, 5). 
neighbor(4, 4).
neighbor(1, 1). 

domain("mapf_domain.lp").
problem("prob2_3.lp").

occurs(3,("left",3,7,5,6),1).
occurs(3,("left",3,6,5,5),2).
occurs(3,("left",3,5,5,4),3).
occurs(3,("down",3,4,5,4),4).
occurs(3,("left",3,4,4,3),5).
occurs(3,("left",3,3,4,2),6).
occurs(3,("down",3,2,4,3),7).
occurs(3,("left",3,2,3,1),8).
occurs(3,("stay",3,1,3),9).
occurs(3,("stay",3,1,3),10).
occurs(3,("down",3,1,3,2),11).
occurs(3,("left",3,1,2,0),12).
occurs(3,("down",3,0,2,1),13).
occurs(3,("up",3,0,1,2),14).
occurs(3,("stay",3,0,2),15).
