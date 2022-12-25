# we can define a function named RK4 to help us

# for the equation 1 after we putting all values into the equation
#d[E]/dt=-1000*E,E(0)=1

#now we may define the function
#n= iteration
#h= step size
#t= initial time 
#E= initial enzyme E
#f = function

# for d[E]/dt=-1000*E,E(0)=1
f<-function(t,E){-1000*E}

RK4<-function(n,h,t,E,f){
  t1<-matrix(0,1,n)
  t1[1]=t
  for (i in 1:n){
    k1=h*f(t1[i],E)
    k2=h*f(t1[i]+0.5*h,E+0.5*k1)
    k3=h*f(t1[i]+0.5*h,E+0.5*k2)
    k4=h*f(t1[i]+h,E+k3)
    E=E+1/6*(k1+2*k2+2*k3+k4)
    print(E)
    t1[i+1]=t1[i]+h
  }
}

#for example:
RK4(2,0.001,0,1,f)
E1= 0.375
E2= 0.140625





# for d[S]/dt=-100*E,E(0)=10
f<-function(t,S){-100*S}

RK4<-function(n,h,t,S,f){
  t1<-matrix(0,1,n)
  t1[1]=t
  for (i in 1:n){
   
    k1=h*f(t1[i],S)
    k2=h*f(t1[i]+0.5*h,S+0.5*k1)
    k3=h*f(t1[i]+0.5*h,S+0.5*k2)
    k4=h*f(t1[i]+h,S+k3)
    S=S+1/6*(k1+2*k2+2*k3+k4)
    print(S)
    t1[i+1]=t1[i]+h
  }
}

#for example:
RK4(5,0.001,0,10,f)
s1= 9.048375
s2= 8.187309
s3= 7.408184
s4= 6.703203
s5= 6.065309




# for d[ES]/dt=1000-750*ES,E(0)=0
f<-function(t,ES){1000-750*ES}

RK4<-function(n,h,t,ES,f){
  t1<-matrix(0,1,n)
  t1[1]=t
  for (i in 1:n){
    
    k1=h*f(t1[i],ES)
    k2=h*f(t1[i]+0.5*h,ES+0.5*k1)
    k3=h*f(t1[i]+0.5*h,ES+0.5*k2)
    k4=h*f(t1[i]+h,ES+k3)
    ES=ES+1/6*(k1+2*k2+2*k3+k4)
    print(ES)
    t1[i+1]=t1[i]+h
  }
}

#for example:
RK4(2,0.001,0,10,f)
ES1= 5.442383
ES2= 3.28152


# for d[P]/dt=0,E(0)=0
f<-function(t,S){(10*S)/(10+S)}

RK4<-function(n,h,t,S,f){
  t1<-matrix(0,1,n)
  t1[1]=t
  for (i in 1:n){
    
    k1=h*f(t1[i],S)
    k2=h*f(t1[i]+0.5*h,S+0.5*k1)
    k3=h*f(t1[i]+0.5*h,S+0.5*k2)
    k4=h*f(t1[i]+h,S+k3)
    S=S+1/6*(k1+2*k2+2*k3+k4)
    print(S)
    t1[i+1]=t1[i]+h
  }
}
RK4(5,0.001,0,10,f)
p1=10.005
p2=10.01
p3=10.01501
p4=10.02001
p5=10.02502