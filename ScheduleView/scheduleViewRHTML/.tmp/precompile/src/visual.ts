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
module powerbi.extensibility.visual.scheduleViewRHTML20015F17080644B8B68EC99ADAA3E464  {
    "use strict";

    import DataViewObjectsModule = powerbi.extensibility.utils.dataview.DataViewObject;
	
	const updateHTMLHead: boolean = false;
    const renderVisualUpdateType: number[] = [
        VisualUpdateType.Resize,
        VisualUpdateType.ResizeEnd,
        VisualUpdateType.Resize + VisualUpdateType.ResizeEnd
    ];

    interface VisualSettingsGeneralParams {
        sorting: string;
        orientation: string;
        colorPalette: string;
        legendCols: string;
    }

    export class Visual implements IVisual {
        private rootElement: HTMLElement;
        private headNodes: Node[];
        private bodyNodes: Node[];
        private settings: VisualSettings;
        private settings_general: VisualSettingsGeneralParams;


        public constructor(options: VisualConstructorOptions) {
            if (options && options.element) {
                this.rootElement = options.element;
            }
            this.headNodes = [];
            this.bodyNodes = [];

            this.settings_general = <VisualSettingsGeneralParams>{
                sorting: "az",
                orientation: "horizontal",
                colorPalette: "Set1",
                legendCols: "auto"
            };
        }

		public update(options: VisualUpdateOptions): void {

            if (!options ||
                !options.type ||
                !options.viewport ||
                !options.dataViews ||
                options.dataViews.length === 0 ||
                !options.dataViews[0]) {
                return;
            }
            const dataView: DataView = options.dataViews[0];
            this.settings = Visual.parseSettings(dataView);
            //RVIZ_IN_PBI_GUIDE:BEGIN:Added to create HTML-based 
            this.updateObjects(dataView.metadata.objects);
            //RVIZ_IN_PBI_GUIDE:END:Added to create HTML-based 
            let payloadBase64: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                payloadBase64 = dataView.scriptResult.payloadBase64;
            }

            if (renderVisualUpdateType.indexOf(options.type) === -1) {
                if (payloadBase64) {
                    this.injectCodeFromPayload(payloadBase64);
                }
            } else {
                this.onResizing(options.viewport);
            }
        }	

        public onResizing(finalViewport: IViewport): void {
			
        }
		
		private injectCodeFromPayload(payloadBase64: string): void {
            // inject HTML from payload, created in R
            // the code is injected to the 'head' and 'body' sections.
            // if the visual was already rendered, the previous DOM elements are cleared
            ResetInjector();

            if (!payloadBase64) {
                return;
            }

            // create 'virtual' HTML, so parsing is easier
            let el: HTMLHtmlElement = document.createElement("html");
            try {
                el.innerHTML = window.atob(payloadBase64);
            } catch (err) {
                return;
            }

            // if 'updateHTMLHead == false', then the code updates the header data only on the 1st rendering
            // this option allows loading and parsing of large and recurring scripts only once.
            if (updateHTMLHead || this.headNodes.length === 0) {
                while (this.headNodes.length > 0) {
                    let tempNode: Node = this.headNodes.pop();
                    document.head.removeChild(tempNode);
                }
                let headList: NodeListOf<HTMLHeadElement> = el.getElementsByTagName("head");
                if (headList && headList.length > 0) {
                    let head: HTMLHeadElement = headList[0];
                    this.headNodes = ParseElement(head, document.head);
                }
            }

            // update 'body' nodes, under the rootElement
            while (this.bodyNodes.length > 0) {
                let tempNode: Node = this.bodyNodes.pop();
                this.rootElement.removeChild(tempNode);
            }
            let bodyList: NodeListOf<HTMLBodyElement> = el.getElementsByTagName("body");
            if (bodyList && bodyList.length > 0) {
                let body: HTMLBodyElement = bodyList[0];
                this.bodyNodes = ParseElement(body, this.rootElement);
            }

            RunHTMLWidgetRenderer();
        }

        private static parseSettings(dataView: DataView): VisualSettings {
            return VisualSettings.parse(dataView) as VisualSettings;
        }


        public updateObjects(objects: DataViewObjects) {
            
            this.settings_general = <VisualSettingsGeneralParams>{
                sorting: DataViewObjectsModule.getValue<string>(objects, 'sorting', "az"),
                orientation: DataViewObjectsModule.getValue<string>(objects, 'orientation', "horizontal"),
                colorPalette: DataViewObjectsModule.getValue<string>(objects, 'colorPalette', "Set1"),
                legendCols: DataViewObjectsModule.getValue<string>(objects, 'legendCols', "auto")
            };

        }

        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): 
			VisualObjectInstance[] | VisualObjectInstanceEnumerationObject {
            let objectName = options.objectName;
            let objectEnumeration = [];

            switch (objectName) {
                case 'settings_general_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            sorting: this.settings_general.sorting,
                            orientation: this.settings_general.orientation,
                            colorPalette: this.settings_general.colorPalette,
                            legendCols: this.settings_general.legendCols
                        },
                        selector: null
                    });
                    break;
            };
			
            return VisualSettings.enumerateObjectInstances(this.settings || VisualSettings.getDefault(), options);
        }
    }
}