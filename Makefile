FC = gfortran
DSRC = src/miev0
DSRCRT3=src/rt3
DSRCRAD=src/radtran
DOBJS = objs
DLIB = lib
DMOD = lib
FFLAGS= -c -J $(DMOD) -I $(DMOD)


all:	$(DLIB)/libmiev0.a $(DLIB)/librt3.a $(DLIB)/libradtran.a


$(DLIB)/libmiev0.a:	$(DOBJS)/ErrPack.o\
			$(DOBJS)/MIEV0.o\
			$(DOBJS)/RDI1MACH.o\
			$(DOBJS)/mathutils.o\
			$(DOBJS)/miev0mod.o
	ar rcs $@ $^


$(DOBJS)/ErrPack.o:	$(DSRC)/ErrPack.f
	$(FC) $(FFLAGS) $< -o $@


$(DOBJS)/MIEV0.o:	$(DSRC)/MIEV0.f
	$(FC) $(FFLAGS) $< -o $@


$(DOBJS)/RDI1MACH.o:	$(DSRC)/RDI1MACH.f
	echo $@
	$(FC) $(FFLAGS) $< -o $@


$(DOBJS)/mathutils.o:	$(DSRC)/mathutils.f03
	echo $@
	$(FC) $(FFLAGS) $< -o $@


$(DOBJS)/miev0mod.o:	$(DSRC)/miev0mod.f03
	echo $@
	$(FC) $(FFLAGS) $< -o $@


$(DLIB)/librt3.a:	$(DOBJS)/radintg3.o\
			$(DOBJS)/radmat.o\
			$(DOBJS)/radscat3.o\
			$(DOBJS)/radtran3.o\
			$(DOBJS)/radutil3.o\
			$(DOBJS)/rt2subs.o
	ar rcs $@ $^
	
$(DOBJS)/radintg3.o:     $(DSRCRT3)/radintg3.f
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/radmat.o:     $(DSRCRT3)/radmat.f  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/radscat3.o:     $(DSRCRT3)/radscat3.f  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/radtran3.o:     $(DSRCRT3)/radtran3.f  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/radutil3.o:     $(DSRCRT3)/radutil3.f  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/rt2subs.o:     $(DSRCRT3)/rt2subs.f  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@


$(DLIB)/libradtran.a:	$(DOBJS)/rayleigh.o\
			$(DOBJS)/mathutils.o\
			$(DOBJS)/aerosol.o\
			$(DOBJS)/atmos.o\
			$(DOBJS)/mainapp.o\
			$(DLIB)/libmiev0.a\
			$(DLIB)/librt3.a
	ar rcs $@ $^
	
$(DOBJS)/mainapp.o:     $(DSRCRAD)/MainApp.f03
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/aerosol.o:     $(DSRCRAD)/aerosol.f03  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/atmos.o:     $(DSRCRAD)/atmos.f03  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/mathutils.o:     $(DSRCRAD)/mathutils.f03  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/prepare.o:     $(DSRCRAD)/prepare.f03  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@

$(DOBJS)/rayleigh.o:     $(DSRCRAD)/rayleigh.f03  
	@echo $@
	$(FC) $(FFLAGS) $< -o $@
