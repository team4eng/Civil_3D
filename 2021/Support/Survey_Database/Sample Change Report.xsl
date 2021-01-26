<!-- 
Filename: Sample Change Report.xsl
Input: survey project name.rep.xml
Purpose: Demonstrate interpretation of .rep.xml
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:msxsl="urn:schemas-microsoft-com:xslt" xmlns:csharp="http://csharp.org" xmlns:fn="http://www.w3.org/2005/xpath-functions">
<xsl:output method="html" encoding="utf-8"/>
<msxsl:script language="C#" implements-prefix="csharp">
<msxsl:using namespace="System" />
<msxsl:using namespace="System.Collections.Generic" />
<![CDATA[
    static Dictionary<string, string> valueMap = new Dictionary<string, string>();
    static void InitDictionary()
    {
        if ( valueMap.Count <= 0 )
        { 
            try
            {
                // Properties
                valueMap.Add("AngleError",                              "AngleError");
                valueMap.Add("AnglePrecision",                          "AnglePrecision");
                valueMap.Add("AngleType",                               "AngleType");
                valueMap.Add("AngleUnit",                               "AngleUnit");
                valueMap.Add("AngularDifference",                       "AngularDifference");
                valueMap.Add("AtmosphericConditionsCorrection",         "AtmosphericConditionsCorrection");
                valueMap.Add("AutoCreateNewDefinitions",                "AutoCreateNewDefinitions");
                valueMap.Add("AutoFigureRecalc",                        "AutoFigureRecalc");
                valueMap.Add("AutoPointNumber",                         "AutoPointNumber");
                valueMap.Add("AutoPointNumbering",                      "AutoPointNumbering");
                valueMap.Add("AutoPointSynch",                          "AutoPointSynch");
                valueMap.Add("Azimuths",                                "Azimuths");
                valueMap.Add("BalanceAngles",                           "BalanceAngles");
                valueMap.Add("BatchFileName",                           "BatchFileName");
                valueMap.Add("CommandEcho",                             "CommandEcho");
                valueMap.Add("ConfidenceInterval",                      "ConfidenceInterval");
                valueMap.Add("CoordinateConvergence",                   "CoordinateConvergence");
                valueMap.Add("CoordinateDifference",                    "CoordinateDifference");
                valueMap.Add("CoordinatePrecision",                     "CoordinatePrecision");
                valueMap.Add("CurvatureAndRefractionCorrection",        "CurvatureAndRefractionCorrection");
                valueMap.Add("DistanceDifference",                      "DistanceDifference");
                valueMap.Add("DistanceType",                            "DistanceType");
                valueMap.Add("DistanceUnit",                            "DistanceUnit");
                valueMap.Add("DittoFeature",                            "DittoFeature");
                valueMap.Add("EDMPrismEccentricityCorrection",          "EDMPrismEccentricityCorrection");
                valueMap.Add("ElevationDifference",                     "ElevationDifference");
                valueMap.Add("ElevationPrecision",                      "ElevationPrecision");
                valueMap.Add("FigureCoordinateEcho",                    "FigureCoordinateEcho");
                valueMap.Add("FigureCourseEcho",                        "FigureCourseEcho");
                valueMap.Add("FiguresNeedRecalc",                       "FiguresNeedRecalc");
                valueMap.Add("HorizontalAdjustmentMethod",              "HorizontalAdjustmentMethod");
                valueMap.Add("HorizontalClosureLimit",                  "HorizontalClosureLimit");
                valueMap.Add("HorizontalCollimationCorrection",         "HorizontalCollimationCorrection");
                valueMap.Add("IncludeToleranceErrorsInCalculations",    "IncludeToleranceErrorsInCalculations");
                valueMap.Add("LatLongPrecision",                        "LatLongPrecision");
                valueMap.Add("LinearPrecision",                         "LinearPrecision");
                valueMap.Add("LoggingEnabled",                          "LoggingEnabled");
                valueMap.Add("LoggingVerboseMode",                      "LoggingVerboseMode");
                valueMap.Add("LSIFileName",                             "LSIFileName");
                valueMap.Add("MaxNumberOfIterations",                   "MaxNumberOfIterations");
                valueMap.Add("NetworkAdjustmentType",                   "NetworkAdjustmentType");
                valueMap.Add("OutputFileName",                          "OutputFileName");
                valueMap.Add("OverwriteStatus",                         "OverwriteStatus");
                valueMap.Add("PerformBlunderDetection",                 "PerformBlunderDetection");
                valueMap.Add("PointCoordinateEcho",                     "PointCoordinateEcho");
                valueMap.Add("PointCourseEcho",                         "PointCourseEcho");
                valueMap.Add("PointsNeedSynch",                         "PointsNeedSynch");
                valueMap.Add("PressureUnit",                            "PressureUnit");
                valueMap.Add("ProjectGUID",                             "ProjectGUID");
                valueMap.Add("RefractionCoefficient",                   "RefractionCoefficient");
                valueMap.Add("Revision",                                "Revision"); 
                valueMap.Add("ScaleFactor",                             "ScaleFactor");
                valueMap.Add("ScaleFactorCorrection",                   "ScaleFactorCorrection");
                valueMap.Add("SdbTransitionalDevelopmentVersion",       "SdbTransitionalDevelopmentVersion");
                valueMap.Add("SdbVersion",                              "SdbVersion");
                valueMap.Add("SeaLevelCorrection",                      "SeaLevelCorrection");
                valueMap.Add("SouthAzimuths",                           "SouthAzimuths");
                valueMap.Add("SpheroidRadius",                          "SpheroidRadius");
                valueMap.Add("SvDataSourceVersion",                     "SvDataSourceVersion");
                valueMap.Add("TargetType",                              "TargetType");
                valueMap.Add("TemperatureUnit",                         "TemperatureUnit");
                valueMap.Add("TraverseAnalysis",                        "TraverseAnalysis");
                valueMap.Add("UpdateSurveyDb",                          "UpdateSurveyDb");
                valueMap.Add("UseAngularDifference",                    "UseAngularDifference");
                valueMap.Add("UseBatchFile",                            "UseBatchFile");
                valueMap.Add("UseCoordinateDifference",                 "UseCoordinateDifference");
                valueMap.Add("UseDistanceDifference",                   "UseDistanceDifference");
                valueMap.Add("UseElevationDifference",                  "UseElevationDifference");
                valueMap.Add("UseOutputFile",                           "UseOutputFile");
                valueMap.Add("VerticalAdjustmentMethod",                "VerticalAdjustmentMethod");
                valueMap.Add("VerticalClosureLimit",                    "VerticalClosureLimit");
                valueMap.Add("VerticalCollimationCorrection",           "VerticalCollimationCorrection");
                valueMap.Add("VerticalType",                            "VerticalType");
                valueMap.Add("WarnForMissingProperties",                "WarnForMissingProperties");
                valueMap.Add("Zone",                                    "Zone");
                
                // Fields
                valueMap.Add("AutomaticUpdate",                         "AutomaticUpdate");
                valueMap.Add("Azimuth",                                 "Azimuth");
                valueMap.Add("AzimuthFlag",                             "AzimuthFlag");
                valueMap.Add("Backsight",                               "Backsight");
                valueMap.Add("BacksightPoint",                          "BacksightPoint");
                valueMap.Add("Boolean",                                 "Boolean");
                valueMap.Add("Breakline",                               "Breakline");
                valueMap.Add("Bulge",                                   "Bulge");
                valueMap.Add("Closed",                                  "Closed");
                valueMap.Add("Code",                                    "Code");
                valueMap.Add("CodesEx",                                 "CodesEx");
                valueMap.Add("CorrectionPressure",                      "CorrectionPressure");
                valueMap.Add("CorrectionTemperature",                   "CorrectionTemperature");
                valueMap.Add("Data0",                                   "Data0");
                valueMap.Add("Data1",                                   "Data1");
                valueMap.Add("Data2",                                   "Data2");
                valueMap.Add("Data3",                                   "Data3");
                valueMap.Add("DateTimeStamp",                           "DateTimeStamp");
                valueMap.Add("Description",                             "Description");
                valueMap.Add("Double",                                  "Double");
                valueMap.Add("Double0",                                 "Double0");
                valueMap.Add("Double1",                                 "Double1");
                valueMap.Add("Double2",                                 "Double2");
                valueMap.Add("Double3",                                 "Double3");
                valueMap.Add("Easting",                                 "Easting");
                valueMap.Add("Elevation",                               "Elevation");
                valueMap.Add("EquipmentAngleUnit",                      "EquipmentAngleUnit");
                valueMap.Add("EquipmentAzimuthStd",                     "EquipmentAzimuthStd");
                valueMap.Add("EquipmentCarrierWaveConstant",            "EquipmentCarrierWaveConstant");
                valueMap.Add("EquipmentCenterStd",                      "EquipmentCenterStd");
                valueMap.Add("EquipmentCircleStd",                      "EquipmentCircleStd");
                valueMap.Add("EquipmentCoordinateStd",                  "EquipmentCoordinateStd");
                valueMap.Add("EquipmentDbName",                         "EquipmentDbName");
                valueMap.Add("EquipmentDescription",                    "EquipmentDescription");
                valueMap.Add("EquipmentDistanceUnit",                   "EquipmentDistanceUnit");
                valueMap.Add("EquipmentEdm",                            "EquipmentEdm");
                valueMap.Add("EquipmentEdmMm",                          "EquipmentEdmMm");
                valueMap.Add("EquipmentEdmOffset",                      "EquipmentEdmOffset");
                valueMap.Add("EquipmentEdmPpm",                         "EquipmentEdmPpm");
                valueMap.Add("EquipmentElevationStd",                   "EquipmentElevationStd");
                valueMap.Add("EquipmentFrequencyRefractiveIndex",       "EquipmentFrequencyRefractiveIndex");
                valueMap.Add("EquipmentHorizontalCollimation",          "EquipmentHorizontalCollimation");
                valueMap.Add("EquipmentLeftAngles",                     "EquipmentLeftAngles");
                valueMap.Add("EquipmentName",                           "EquipmentName");
                valueMap.Add("EquipmentPointingStd",                    "EquipmentPointingStd");
                valueMap.Add("EquipmentPrismConstant",                  "EquipmentPrismConstant");
                valueMap.Add("EquipmentPrismOffset",                    "EquipmentPrismOffset");
                valueMap.Add("EquipmentPrismStd",                       "EquipmentPrismStd");
                valueMap.Add("EquipmentTargetStd",                      "EquipmentTargetStd");
                valueMap.Add("EquipmentTheodoliteStd",                  "EquipmentTheodoliteStd");
                valueMap.Add("EquipmentTiltedPrism",                    "EquipmentTiltedPrism");
                valueMap.Add("EquipmentVerticalAngleType",              "EquipmentVerticalAngleType");
                valueMap.Add("EquipmentVerticalCircleStd",              "EquipmentVerticalCircleStd");
                valueMap.Add("EquipmentVerticalCollimation",            "EquipmentVerticalCollimation");
                valueMap.Add("F2FConventionName",                       "F2FConventionName");
                valueMap.Add("Face1",                                   "Face1");
                valueMap.Add("Face2",                                   "Face2");
                valueMap.Add("FigureId",                                "FigureId");
                valueMap.Add("FigurePrefixDbName",                      "FigurePrefixDbName");
                valueMap.Add("FilePath",                                "FilePath");
                valueMap.Add("FinalForesight",                          "FinalForesight");
                valueMap.Add("FirstPtX",                                "FirstPtX");
                valueMap.Add("FirstPtY",                                "FirstPtY");
                valueMap.Add("FlagsEx",                                 "FlagsEx");
                valueMap.Add("Forward",                                 "Forward");
                valueMap.Add("GUID",                                    "GUID");
                valueMap.Add("Id",                                      "Id");
                valueMap.Add("ImportEventId",                           "ImportEventId");
                valueMap.Add("ImportEventPrimary",                      "ImportEventPrimary");
                valueMap.Add("ImportType",                              "ImportType");
                valueMap.Add("InitialBacksight",                        "InitialBacksight");
                valueMap.Add("InitialObservationId",                    "InitialObservationId");
                valueMap.Add("InitialStation",                          "InitialStation");
                valueMap.Add("Instrument",                              "Instrument");
                valueMap.Add("Integer",                                 "Integer");
                valueMap.Add("IsControlPoint",                          "IsControlPoint");
                valueMap.Add("IsFakePoint",                             "IsFakePoint");
                valueMap.Add("IsGenerated",                             "IsGenerated");
                valueMap.Add("IsNessPoint",                             "IsNessPoint");
                valueMap.Add("IsSetupPoint",                            "IsSetupPoint");
                valueMap.Add("LandXML",                                 "LandXML");
                valueMap.Add("LastAz",                                  "LastAz");
                valueMap.Add("LastDist",                                "LastDist");
                valueMap.Add("LastPtX",                                 "LastPtX");
                valueMap.Add("LastPtY",                                 "LastPtY");
                valueMap.Add("Layer",                                   "Layer");
                valueMap.Add("ListName",                                "ListName");
                valueMap.Add("ListOwnerId",                             "ListOwnerId");
                valueMap.Add("LotLine",                                 "LotLine");
                valueMap.Add("Mask",                                    "Mask");
                valueMap.Add("Monument",                                "Monument");
                valueMap.Add("Name",                                    "Name");
                valueMap.Add("NeedsUpdate",                             "NeedsUpdate");
                valueMap.Add("NetworkId",                               "NetworkId");
                valueMap.Add("NodeNumber",                              "NodeNumber");
                valueMap.Add("Northing",                                "Northing");
                valueMap.Add("Number",                                  "Number");
                valueMap.Add("ObservationType",                         "ObservationType");
                valueMap.Add("Order",                                   "Order");
                valueMap.Add("Orientation",                             "Orientation");
                valueMap.Add("OriginalName",                            "OriginalName");
                valueMap.Add("OriginalNumber",                          "OriginalNumber");
                valueMap.Add("ParseUnit",                               "ParseUnit");
                valueMap.Add("Point",                                   "Point");
                valueMap.Add("Point0",                                  "Point0");
                valueMap.Add("Point1",                                  "Point1");
                valueMap.Add("PointCount",                              "PointCount");
                valueMap.Add("PointFormatName",                         "PointFormatName");
                valueMap.Add("PointIdOffset",                           "PointIdOffset");
                valueMap.Add("ProcessLinework",                         "ProcessLinework");
                valueMap.Add("ProcessLineworkSequence",                 "ProcessLineworkSequence");
                valueMap.Add("ProcessOrder",                            "ProcessOrder");
                valueMap.Add("Property",                                "Property");
                //valueMap.Add("Revision",                                "Revision"); // duplicate
                //valueMap.Add("ScaleFactor",                             "ScaleFactor"); // duplicate
                valueMap.Add("SetupId",                                 "SetupId");
                valueMap.Add("Site",                                    "Site");
                valueMap.Add("SourceUnit",                              "SourceUnit");
                valueMap.Add("Station",                                 "Station");
                valueMap.Add("StationPoint",                            "StationPoint");
                valueMap.Add("Stations",                                "Stations");
                valueMap.Add("Std0",                                    "Std0");
                valueMap.Add("Std1",                                    "Std1");
                valueMap.Add("Std2",                                    "Std2");
                valueMap.Add("Std3",                                    "Std3");
                valueMap.Add("String",                                  "String");
                valueMap.Add("Style",                                   "Style");
                valueMap.Add("TableName",                               "TableName");
                valueMap.Add("Theodolite",                              "Theodolite");
                valueMap.Add("TypeFlags",                               "TypeFlags");
                valueMap.Add("UsePointIdOffset",                        "UsePointIdOffset");
                valueMap.Add("UserAzimuth",                             "UserAzimuth");
                valueMap.Add("UserName",                                "UserName");
                valueMap.Add("X",                                       "X");
                valueMap.Add("Y",                                       "Y");
                valueMap.Add("Z",                                       "Z");
            }
            catch (ArgumentException)
            {
                // An element with this key already exists. If you can't identify the duplicate, 
                // try pasting the block of code above into a spreadsheet and sort to find duplicates.
            }
        }
        return;
    }
    
    static public string MapFieldText(string value)
    {
        InitDictionary();
	    string subValue;
	    if ( !valueMap.TryGetValue( value, out subValue) )
	    {
		    subValue = value;
	    }
	    return subValue;
    }
]]>
</msxsl:script>

<xsl:template match="/">
    <xsl:apply-templates />
</xsl:template>

    <xsl:template match="AeccSvLog">
        <html>
            <head>
                <title>
                    Survey Change Report - <xsl:value-of select="@logfile"/> - <xsl:value-of select="@datetime"/>
                </title>
            </head>
            <body>
                <table bgcolor="#000000" width="100%">
                    <tr>
                        <td width="*" align="center" valign="middle">
                            <font color="#FFFFFF">
                                <H2>Survey Database Change Report</H2>
                            </font>
                        </td>
                    </tr>
                    <tr>
                        <table bgcolor="#000000" width="100%">
                            <tr>

                                <td width="150">
                                    <font color="#FFFFFF">Generated:</font>
                                </td>
                                <td width="*">
                                    <font color="#FFFFFF">
                                        <xsl:value-of select="@datetime"/>
                                    </font>
                                </td>

                            </tr>
                            <tr>
                                <td>
                                    <font color="#FFFFFF">By user:</font>
                                </td>
                                <td>
                                    <font color="#FFFFFF">
                                        <xsl:value-of select="@user"/>
                                    </font>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <font color="#FFFFFF">Logfile:</font>
                                </td>
                                <td>
                                    <font color="#FFFFFF">
                                        <xsl:value-of select="@logfile"/>
                                    </font>
                                </td>
                            </tr>
                        </table>
                    </tr>
                </table>
                <table width="100%" border="1" cellpadding="0" cellspacing="0">
                    <tr>
                        <td width="40">
                            User
                        </td>
                        <td width="40">
                            Level
                        </td>
                        <td width="150">
                            Date/Time
                        </td>
                        <td width="40">
                            Source
                        </td>
                        <td width="*">
                            Description
                        </td>
                    </tr>
                    <xsl:apply-templates />
                </table>
            </body>
        </html>
    </xsl:template>

    <xsl:template match="AeccSvLogNesting">
        <tr>
                <td width="40" align="left" valign="top">
                    <xsl:value-of select="@user"/>
                </td>
                <td width="40" align="right" valign="top">
                    <xsl:value-of select="@level"/>
                </td>
                <td width="150" align="left" valign="top">
                    <xsl:value-of select="@datetime"/>
                </td>
                <td width="40" align="left" valign="top">
                    <xsl:value-of select="@source"/>
                </td>
                <td width="*" align="left" valign="top">
                    
                        <table width="100%" border="1" cellpadding="0" cellspacing="0">
                        <tr>
                            <td width="40">
                                User
                            </td>
                            <td width="40">
                                Level
                            </td>
                            <td width="150">
                                Date/Time
                            </td>
                            <td width="40">
                                Source
                            </td>
                            <td width="*">
                                Description
                            </td>
                        </tr>
                        <xsl:apply-templates />
                    </table>
                </td>
        </tr>
    </xsl:template>

    <xsl:template match="AeccSvLogEvent">
        <tr>
            <td width="40" align="left" valign="top">
                <xsl:value-of select="@user"/>
            </td>
            <td width="40" align="right" valign="top">
                <xsl:value-of select="@level"/>
            </td>
            <td width="150" align="left" valign="top">
                <xsl:value-of select="@datetime"/>
            </td>
            <td width="40" align="left" valign="top">
                <xsl:value-of select="@source"/>
            </td>
            <td width="*" align="left" valign="top">
                <xsl:apply-templates />
            </td>
        </tr>
    </xsl:template>

    <xsl:template match="AeccSvFieldReport">
        <table width="100%">
            <tr>
                <td width="40" align="left" valign="top">
                </td>
                <td width="250" align="left" valign="top">
                    <b>Field</b>
                </td>
                <td width="*" align="left" valign="top">
                    <b>Value</b>
                </td>
            </tr>
            <xsl:apply-templates />
        </table>
    </xsl:template>

    <xsl:template match="AeccSvFieldChangeReport">
        <table width="100%">
            <tr>
                <td width="40" align="left" valign="top">
                </td>
                <td width="250" align="left" valign="top">
                    <b>Field</b>
                </td>
                <td width="200" align="left" valign="top">
                    <b>Old Value</b>
                </td>
                <td width="*" align="left" valign="top">
                    <b>New Value</b>
                </td>
            </tr>
            <xsl:apply-templates />
        </table>
    </xsl:template>

    <xsl:template match="AeccSvField">
        <tr>
            <td width="40" align="left" valign="top">
            </td>
            <td width="250" align="left" valign="top">
                <xsl:value-of select="csharp:MapFieldText(@field)"/>
            </td>
            <td width="*" align="left" valign="top">
                <xsl:value-of select="@value"/>
            </td>
        </tr>
        <xsl:apply-templates />
    </xsl:template>

    <xsl:template match="AeccSvFieldChange">
        <tr>
            <td width="40" align="left" valign="top">
            </td>
            <td width="250" align="left" valign="top">
                <xsl:value-of select="csharp:MapFieldText(@field)"/>
            </td>
            <td width="200" align="left" valign="top">
                <xsl:value-of select="@oldvalue"/>
            </td>
            <td width="*" align="left" valign="top">
                <xsl:value-of select="@newvalue"/>
            </td>
        </tr>
        <xsl:apply-templates />
    </xsl:template>

    <xsl:template match="AeccSvLogTiming">
        <tr>
            <td width="40" align="left" valign="top">
            </td>
            <td width="40" align="right" valign="top">
            </td>
            <td width="150" align="left" valign="top">
            </td>
            <td width="40" align="left" valign="top">
            </td>
            <td width="*" align="left" valign="top">
                <b>
                    Timing: <xsl:value-of select="@ticks"/> (ticks)
                </b>
            </td>
        </tr>
    </xsl:template>

</xsl:stylesheet>
