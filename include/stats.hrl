-record (property_stats, {
	pairsCount,
	avgObjectOccurence
	%histogram % distribution of objects values
	}).
	
-record (class_stats, {
	membersCount }).
	
-record (stats, {
	triplesCount,
	avgObjectOccurence,
	properties, % both are dictionaries of property_stats and class_stats respectively
	classes }).
	
avgOccurrence( 0, Occ ) ->
	Occ;
avgOccurrence( Occ, 0 ) ->
	Occ;
avgOccurrence( Occ1, Occ2 ) ->
	(Occ1 + Occ2) /2.


statsDictMerge( _, #property_stats{ pairsCount = PC1, avgObjectOccurence = AOC1},
					#property_stats{ pairsCount = PC2, avgObjectOccurence = AOC2 } ) ->
	#property_stats{ 
		pairsCount = PC1 + PC2, 
		avgObjectOccurence = avgOccurrence( AOC1, AOC2 ) };
		
statsDictMerge( _, #class_stats{ membersCount = MC1}, #class_stats{ membersCount = MC2 } ) ->
	#class_stats{ membersCount = MC1 + MC2 }.