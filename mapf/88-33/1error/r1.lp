agent(1, 1).
neighbor(2, 2).
neighbor(3, 3). 

%domain("mapf_domain.lp").

domain("../../mapf_domain.lp").
problem("prob1_1.lp").

occurs(1,("left",1,7,7,6),1).
occurs(1,("left",1,6,7,5),2).
occurs(1,("down",1,5,7,6),3).
occurs(1,("down",1,5,6,5),4).
occurs(1,("down",1,5,5,4),5).
occurs(1,("left",1,5,4,4),6).
occurs(1,("left",1,4,4,3),9).
occurs(1,("left",1,3,4,2),10).
occurs(1,("left",1,2,4,1),11).
occurs(1,("down",1,1,4,3),12).
occurs(1,("up",1,1,3,4),13).
occurs(1,("left",1,1,4,0),14).
occurs(1,("down",1,0,4,3),15).
