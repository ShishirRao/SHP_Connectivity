len = seq(from=0, to=1000,by = 50)

DCIp = ((100*((len)/1000)^2)+(100*((1000-len)/1000)^2))
DCIpInv = 100 - DCIp 

df = data.frame(len=len,DCIp = DCIp)

plot(DCIpInv ~ len)

DCIdInv = 100 - (100*((len)/1000))

plot(DCIdInv ~ len)

Area = seq(from=0, to=10000,by = 500)

CAFI =  100 *(10000-Area/10000)

plot(CAFI ~ Area)

c11 = c22 = 1
c12 = c21 = 0.5
l1 = 5
l2 = 25
L = l1 + l2

C= c11 *  (l1/L)^2 + c12*(l1*l2/L^2) + c21*(l1*l2)/L^2 + c22*(l2/L)^2


c11 = c22 = 1
c12 = c21 = 0.5
l1 = 10
l2 = 20
L = l1 + l2

DCIp= c11 *  (l1/L)^2 + c12*(l1*l2/L^2) + c21*(l1*l2)/L^2 + c22*(l2/L)^2

c1 = 0.5
c2 = 1
DCId = c1 * l1/L + c2 * l2/L


c11  = c33 = 1
c22 = 0

c12 = c21 = 0
c23 = c32 = 0
c13 = c31 = 0

l1 = 5
l2 = 5
l3 = 20

L = l1 + l2 + l3

C = c11*(l1/L)^2 + c12*(l1*l2/L^2) + c13*(l1*l3/L^2)+
  c21*(l2*l1)/L^2 + c22*(l2/L)^2 + c23*(l2*l3/L^2)+
  c31*(l3*l1)/L^2 + c32*(l3*l2/L^2)+ c33*(l3/L)^2

l2 = 0


