#.........................................................................




kmethod3<-function(k )
{	
	m=NULL;
	m=iris[1:100,];
	m=m[sample(1:100,replace=F ), ];
	training<-m[1:80,];
	test<-m[81:100,];
	
	training1<-training[,1:4];
	test1<-test[,1:4];
	
	#									Extract the data from iris object.								
	#................................................................
	
	
	i=NULL;
	j=NULL;
	
	m=NULL;
	
	l=NULL;
	verified=NULL;
for (i in 1:20  )
	{	
		c<-NULL
		for (j in 1:80 )
		{
			m= test1[i,]-training1[j,];						#.... Euclidean Distance calculation.  
			m=m*m;
			m=sum(m);
			m=sqrt(m);
			c<-c(c,m);
	
		}


		c<-sort(c,index.return=TRUE)$ix;					#... Sorts the Euclidean distance 
															#... and gives index as o/p.
		c<-c[1:k];											
		srtclass<-training[c,5];							#... Extracting the training examples.							
		count1<-0;
		count2<-0;
		
		for(l in 1:k )										#... Finding the mejority class.
		{
			if(srtclass[l]=="setosa" )
			{
				count1=count1+1;
			}
			if(srtclass[l]=="versicolor" )	
			{
				count2=count2+1;
			}

		}

		if( count1 > count2)								#... Verifying the clas and assigning it test example.
		{	verified<-c(verified,"setosa");
		}
		else
		{
			verified<-c(verified,"versicolor");	
		}
		

		#return (c);
		#classes<-training[which( training1[i,]<- c)] ; 	
	}	
	verified<-as.character(verified);

	test<-cbind(test,verified);
	#.................................................................

	# Accuracy calculation.
	count3<-0
	for ( i in 1:20 )
	{
		if( test[i,5]==verified[i] )
		{
			count3<-count3+1;

		}
	}

	accuracy<- (count3/20)*100;

	cat('Accuracy:')
	
 return (list(accuracy,test));
	
}


