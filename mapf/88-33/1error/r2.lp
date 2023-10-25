agent(2, 2).
neighbor(1, 1).
neighbor(3, 3). 

%domain("mapf_domain.lp").

domain("../../mapf_domain.lp").
problem("prob1_2.lp").

occurs(2,("right",2,0,0,1),1).
occurs(2,("right",2,1,0,2),2).
occurs(2,("right",2,2,0,3),3).
occurs(2,("up",2,3,0,1),4).
occurs(2,("up",2,3,1,2),5).
occurs(2,("up",2,3,2,3),6).
occurs(2,("up",2,3,3,4),7).
occurs(2,("right",2,3,4,4),8).
occurs(2,("right",2,4,4,5),9).
occurs(2,("down",2,5,4,3),10).
occurs(2,("down",2,5,3,2),11).
occurs(2,("down",2,5,2,1),12).
occurs(2,("down",2,5,1,0),13).
occurs(2,("right",2,5,0,6),14).
occurs(2,("right",2,6,0,7),15).
