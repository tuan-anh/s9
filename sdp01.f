ccc   to read data from casa & gildas and look at noise
      logical sea,nwa
      call hlimap(20000,'SDP')
      call hbook1(11,'',100,-5.E-5,1.5E-4,0.)
c      call hbook1(11,'',100,-5.E-5,4.5E-4,0.)
      call hbook1(12,'',100,-5.E-5,4.5E-4,0.)
      call hbook1(13,'',100,-1.E-3,2.E-3,0.)
      call hbook1(14,'',100,-1.E-3,2.E-3,0.)
c      call hbook2(20,'',71.,-.8875,.8875,71.,-.8875,.8875,0.)
      call hbook2(21,'',71.,-.8875,.8875,71.,-.8875,.8875,0.)
      call hbook2(22,'',71.,-.8875,.8875,71.,-.8875,.8875,0.)
      call hbook2(23,'',71.,-.8875,.8875,71.,-.8875,.8875,0.)
      call hbook1(31,'',43.,0.,43.,0.)
      call hbook1(32,'',43.,0.,43.,0.)  
      pi=2.*acos(0.)
      pxsize=0.005
      ixcen=1201
      iycen=1201
      open(2,file='sdp9mask-360-360.dat')
 8    continue
      read(2,*,end=9)j,i,aa
      x=(float(i)-ixcen)*pxsize
      y=(float(j)-ixcen)*pxsize
      call hf2(20,x,y,aa)
      go to 8
 9    continue
      close(2)
      open(2,file='continuum15-360-360.dat')
 11   continue
      read(2,*,end=12)j,i,aa
      call hf1(11,aa,1.)
      x=(float(i)-ixcen)*pxsize
      y=(float(j)-ixcen)*pxsize
      call hf2(21,x,y,aa)
      go to 11
 12   continue
      close(2)
      
      open(2,file='gildas-cont3-360-360.dat')
 13   continue
      read(2,*,end=14)j,i,aa
      call hf1(12,aa,1.)
      x=(float(i)-ixcen)*pxsize
      y=(float(j)-ixcen)*pxsize
c      call hf2(22,x,y,aa)
      go to 13
 14   continue
      close(2)

      open(2,file='ltry5-360-360.dat')
 15   continue
      read(2,*,end=16)k,j,i,aa
      call hf1(13,aa,1.)
      x=(float(i)-ixcen)*pxsize
      y=(float(j)-ixcen)*pxsize
      call hf2(22,x,y,aa)
      go to 15
 16   continue
      close(2)
      
      open(2,file='line9-360-360.dat')
ccc   channel 19th/43 here has the same LSR with
ccc   channel 16th/31 from GILDAS
      snoise=1.E-4
 17   continue
      read(2,*,end=18)k,j,i,aa
      call hf1(14,aa,1.)
      x=(float(i)-ixcen)*pxsize
      y=(float(j)-ixcen)*pxsize
      r=sqrt(x**2+y**2)
      omega=atan2(y,x)
      if(omega.lt.0.)omega=omega+2.*pi
      omdeg=omega*180./pi
      sea=.false.
      nwa=.false.
      if(r.gt..55.and.r.lt..8.and.omdeg.gt.180.
     &     .and.omdeg.lt.270.)sea=.true.
      if(r.gt..55.and.r.lt..8.and.omdeg.gt.40.
     &     .and.omdeg.lt.70.)nwa=.true.
      if(sea.or.nwa.and.aa.gt.snoise)call hf2(23,x,y,aa)
      if(nwa)call hf1(31,float(k)-.5,aa)
      if(sea)call hf1(32,float(k)-.5,aa)
c      if(aa.gt.3.*snoise)call hf2(22,x,y,aa)
      go to 17
 18   continue
      close(2)      

      stop
      end
