

import pandas as pd


frame = pd.DataFrame()

#define ow_ns dataframe
ow = pd.read_csv("SensorR/ow_ns.csv", index_col = 0)
ow_frame = pd.DataFrame(ow)

ow_frame2 = ow_frame.reindex(['temp_n_main', 'rh_n_main', 'wet_n_main', 'temp_n_e', 'rh_n_e', 'wet_n_e', 'temp_n_m', 'rh_n_m',
       'wet_n_m', 'temp_n_x', 'rh_n_x', 'wet_n_x',
       'temp_s_a11', 'rh_s_a11', 'wet_s_a11', 'temp_s_main', 'rh_s_main', 'wet_s_main', 
       'temp_s_a16', 'rh_s_a16', 'wet_s_a16', 'temp_s_g',
       'rh_s_g', 'wet_s_g', 'temp_s_y', 'rh_s_y', 'wet_s_y'])

print(ow_frame.columns) 