print('###################################################')
print('Gather and prepare data from preprocessing...')
print('###################################################\n')
import src.d00_preprocessing.gathering
import src.d00_preprocessing.matching_daily
import src.d00_preprocessing.matching_intraday


print('###################################################')
print('Run figure results...')
print('###################################################\n')
import src.d01_figures.daily
import src.d01_figures.intraday

print('###################################################')
print('Run table results...')
print('###################################################\n')
import src.d02_results.daily
import src.d02_results.intraday
import src.d02_results.russiaukraine
import src.d02_results.aerospacedefense