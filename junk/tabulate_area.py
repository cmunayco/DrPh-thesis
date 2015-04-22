import arcpy
arcpy.env.overwriteOutput = True
arcpy.CheckOutExtension('Spatial')
regions = '/Users/cvmunayco/Documents/Repository/DrPh-thesis/junk/qgis_sample_data/shapefiles/regions.shp'
landcover = '/Users/cvmunayco/Documents/Repository/DrPh-thesis/junk/qgis_sample_data/raster/landcover.img'
outtable = '/Users/cvmunayco/Documents/Repository/DrPh-thesis/junk/out.dbf'
arcpy.AddField_management(landcover, 'value2', 'INTEGER')
arcpy.CalculateField_management(landcover, 'value2', '!Value!','PYTHON_9.3')
arcpy.gp.TabulateArea_sa(regions, 'NAME_2', landcover, 'value2', outtable)
    