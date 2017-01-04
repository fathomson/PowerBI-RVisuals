var powerbi;
(function (powerbi) {
    var extensibility;
    (function (extensibility) {
        var visual;
        (function (visual) {
            var PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051;
            (function (PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051) {
                function getValue(objects, objectName, propertyName, defaultValue) {
                    if (objects) {
                        var object = objects[objectName];
                        if (object) {
                            var property = object[propertyName];
                            if (property !== undefined) {
                                return property;
                            }
                        }
                    }
                    return defaultValue;
                }
                PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.getValue = getValue;
                function getFillValue(objects, objectName, propertyName, defaultValue) {
                    if (objects) {
                        var object = objects[objectName];
                        if (object) {
                            var fill = object[propertyName];
                            if (fill !== undefined && fill.solid !== undefined && fill.solid.color !== undefined) {
                                return fill.solid.color;
                            }
                        }
                    }
                    return defaultValue;
                }
                PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.getFillValue = getFillValue;
            })(PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051 = visual.PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051 || (visual.PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051 = {}));
        })(visual = extensibility.visual || (extensibility.visual = {}));
    })(extensibility = powerbi.extensibility || (powerbi.extensibility = {}));
})(powerbi || (powerbi = {}));
/*
 *  Power BI Visual CLI
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */
var powerbi;
(function (powerbi) {
    var extensibility;
    (function (extensibility) {
        var visual;
        (function (visual) {
            var PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051;
            (function (PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051) {
                // To allow this scenario you should first the following JSON definition to the capabilities.json file
                // under the "objects" property:
                // "settings": {
                //     "displayName": "Visual Settings",
                //     "description": "Visual Settings Tooltip",
                //     "properties": {
                //         "lineColor": {
                //         "displayName": "Line Color",
                //         "type": { "fill": { "solid": { "color": true }}}
                //         }
                //     }
                // }
                var Visual = (function () {
                    // Snippet for defining the member property which will hold the property pane values
                    /*private settings: VisualSettings;*/
                    function Visual(options) {
                        this.imageDiv = document.createElement('div');
                        this.imageDiv.className = 'rcv_autoScaleImageContainer';
                        options.element.appendChild(this.imageDiv);
                        this.imageElement = document.createElement('img');
                        this.imageElement.className = 'rcv_autoScaleImage';
                        this.imageDiv.appendChild(this.imageElement);
                        this.settings = {
                            sorting: "az",
                            orientation: "horizontal",
                            colorPalette: "Set1"
                        };
                    }
                    Visual.prototype.update = function (options) {
                        var dataViews = options.dataViews;
                        if (!dataViews || dataViews.length === 0)
                            return;
                        var dataView = dataViews[0];
                        if (!dataView || !dataView.metadata)
                            return;
                        //this.updateObjects(dataView.metadata.objects);
                        this.settings = {
                            sorting: PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.getValue(dataView.metadata.objects, 'settings', 'sorting', 'az'),
                            orientation: PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.getValue(dataView.metadata.objects, 'settings', 'orientation', 'horizontal'),
                            colorPalette: PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.getValue(dataView.metadata.objects, 'settings', 'colorPalette', 'Set1')
                        };
                        var imageUrl = null;
                        if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                            imageUrl = "data:image/png;base64," + dataView.scriptResult.payloadBase64;
                        }
                        if (imageUrl) {
                            this.imageElement.src = imageUrl;
                        }
                        else {
                            this.imageElement.src = null;
                        }
                        this.onResizing(options.viewport);
                    };
                    Visual.prototype.onResizing = function (finalViewport) {
                        this.imageDiv.style.height = finalViewport.height + 'px';
                        this.imageDiv.style.width = finalViewport.width + 'px';
                    };
                    /**
                     * This function gets called by the update function above. You should read the new values of the properties into
                     * your settings object so you can use the new value in the enumerateObjectInstances function below.
                     *
                     * Below is a code snippet demonstrating how to expose a single property called "lineColor" from the object called "settings"
                     * This object and property should be first defined in the capabilities.json file in the objects section.
                     * In this code we get the property value from the objects (and have a default value in case the property is undefined)
                     */
                    Visual.prototype.updateObjects = function (objects) {
                        this.settings = {
                            sorting: PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.getValue(objects, 'settings', 'sorting', 'az'),
                            orientation: PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.getValue(objects, 'settings', 'orientation', 'horizontal'),
                            colorPalette: PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.getValue(objects, 'settings', 'colorPalette', 'Set1')
                        };
                    };
                    /**
                     * This function gets called for each of the objects defined in the capabilities files and allows you to select which of the
                     * objects and properties you want to expose to the users in the property pane.
                     *
                     * Below is a code snippet for a case where you want to expose a single property called "lineColor" from the object called "settings"
                     * This object and property should be first defined in the capabilities.json file in the objects section.
                     */
                    Visual.prototype.enumerateObjectInstances = function (options) {
                        var objectName = options.objectName;
                        var objectEnumeration = [];
                        switch (objectName) {
                            case 'settings':
                                objectEnumeration.push({
                                    objectName: objectName,
                                    properties: {
                                        sorting: this.settings.sorting,
                                        orientation: this.settings.orientation,
                                        colorPalette: this.settings.colorPalette
                                    },
                                    selector: null
                                });
                                break;
                        }
                        ;
                        return objectEnumeration;
                    };
                    return Visual;
                }());
                PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.Visual = Visual;
            })(PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051 = visual.PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051 || (visual.PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051 = {}));
        })(visual = extensibility.visual || (extensibility.visual = {}));
    })(extensibility = powerbi.extensibility || (powerbi.extensibility = {}));
})(powerbi || (powerbi = {}));
var powerbi;
(function (powerbi) {
    var visuals;
    (function (visuals) {
        var plugins;
        (function (plugins) {
            plugins.PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051 = {
                name: 'PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051',
                displayName: 'ScheduleView',
                class: 'Visual',
                version: '1.0.2',
                apiVersion: '1.3.0',
                create: function (options) { return new powerbi.extensibility.visual.PBI_CV_658497A5_7E34_4F94_A51F_4E2213BB9051.Visual(options); },
                custom: true
            };
        })(plugins = visuals.plugins || (visuals.plugins = {}));
    })(visuals = powerbi.visuals || (powerbi.visuals = {}));
})(powerbi || (powerbi = {}));
//# sourceMappingURL=visual.js.map