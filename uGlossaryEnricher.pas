unit uGlossaryEnricher;

{
  Glossary Enrichment System for Pascal Code Search

  Purpose: Enriches Pascal code chunks with Spanish-English translation metadata
           before embedding generation to improve AI search across mixed-language codebases.

  Architecture: Index-time enrichment (zero query overhead)

  Usage:
    Enricher := TGlossaryEnricher.Create;
    try
      EnrichedCode := Enricher.EnrichChunk(OriginalCode, 'FicharEstaFase', 'procedure');
      DomainTags := Enricher.ExtractDomainTags(OriginalCode);
      DetectedTerms := Enricher.GetDetectedTerms(OriginalCode);
    finally
      Enricher.Free;
    end;
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.RegularExpressions;

type
  /// <summary>
  /// Represents a glossary term with translations and domain tags
  /// </summary>
  TGlossaryTerm = record
    SpanishTerm: string;
    EnglishTerms: TArray<string>;
    Context: string;
    DomainTags: TArray<string>;
  end;

  /// <summary>
  /// Enriches Pascal code chunks with translation metadata for improved semantic search
  /// </summary>
  TGlossaryEnricher = class
  private
    FTerms: TDictionary<string, TGlossaryTerm>;
    FAbbreviations: TDictionary<string, string>;

    procedure LoadGlossaryFromConsts;
    function DetectSpanishTerms(const Code: string): TArray<string>;
    function BuildEnrichmentMetadata(const Terms: TArray<string>): string;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Enriches Pascal code chunk with translation metadata for embedding
    /// Returns: Original code + metadata block (if Spanish terms detected)
    /// </summary>
    function EnrichChunk(const OriginalCode: string;
                         const ChunkName: string;
                         const ChunkType: string): string;

    /// <summary>
    /// Extracts domain tags for a code chunk (CSV format)
    /// Example: "TimeTracking,ShopFloorControl,LaborManagement"
    /// </summary>
    function ExtractDomainTags(const Code: string): string;

    /// <summary>
    /// Gets Spanish terms detected in code (JSON array format)
    /// Example: ["Fichar", "Operario"]
    /// </summary>
    function GetDetectedTerms(const Code: string): string;

    /// <summary>
    /// Expands abbreviation to full form (if known)
    /// Example: "Maq" -> "Máquinas"
    /// </summary>
    function ExpandAbbreviation(const Abbrev: string): string;

    /// <summary>
    /// Returns number of terms in glossary
    /// </summary>
    function TermCount: Integer;
  end;

implementation

uses
  System.JSON;

{ TGlossaryEnricher }

constructor TGlossaryEnricher.Create;
begin
  inherited;
  FTerms := TDictionary<string, TGlossaryTerm>.Create;
  FAbbreviations := TDictionary<string, string>.Create;
  LoadGlossaryFromConsts;
end;

destructor TGlossaryEnricher.Destroy;
begin
  FTerms.Free;
  FAbbreviations.Free;
  inherited;
end;

procedure TGlossaryEnricher.LoadGlossaryFromConsts;
var
  Term: TGlossaryTerm;
begin
  // ============================================================
  // UNIFIED GLOSSARY (All Modules Merged)
  // ============================================================
  // Total Terms: 361 (deduplicated from 7 modules)
  // Modules: TCConta, Gestion2000, Clientes, Producción, VCL, Almacen, Proyectos
  // Duplicate terms merged with combined English translations and domain tags
  // ============================================================

  // ALTO
  Term.SpanishTerm := 'ALTO';
  Term.EnglishTerms := TArray<string>.Create(
    'Height',
    'ProductHeight',
    'ItemHeight',
    'Dimension',
    'HeightDimension',
    'HeightSpec'
  );
  Term.Context := 'Product height dimension for storage and shipping planning. Non-incrementable (=) class.';
  Term.DomainTags := TArray<string>.Create(
    'ProductSpecifications',
    'WarehouseManagement',
    'ShippingManagement',
    'SpacePlanning'
  );
  FTerms.Add('ALTO', Term);

  // ANCHO
  Term.SpanishTerm := 'ANCHO';
  Term.EnglishTerms := TArray<string>.Create(
    'Width',
    'ProductWidth',
    'ItemWidth',
    'Dimension',
    'WidthDimension',
    'WidthSpec'
  );
  Term.Context := 'Product width dimension for storage and shipping planning. Non-incrementable (=) class.';
  Term.DomainTags := TArray<string>.Create(
    'ProductSpecifications',
    'WarehouseManagement',
    'ShippingManagement',
    'SpacePlanning'
  );
  FTerms.Add('ANCHO', Term);

  // ============================================================
  // Category: FIXED ASSETS
  // ============================================================

  // Activos
  Term.SpanishTerm := 'Activos';
  Term.EnglishTerms := TArray<string>.Create(
    'FixedAssets',
    'Assets',
    'CapitalAssets',
    'PropertyPlantEquipment',
    'FixedAssetRegister'
  );
  Term.Context := 'Fixed assets or capital assets. Tracked separately with depreciation schedules and posted to accounting.';
  Term.DomainTags := TArray<string>.Create(
    'FixedAssets',
    'AssetManagement',
    'Depreciation',
    'Accounting'
  );
  FTerms.Add('Activos', Term);

  // Actualizar
  Term.SpanishTerm := 'Actualizar';
  Term.EnglishTerms := TArray<string>.Create(
    'Update',
    'Refresh',
    'Synchronize',
    'Modify',
    'Change'
  );
  Term.Context := 'Update production data, inventory, or system records';
  Term.DomainTags := TArray<string>.Create(
    'DataManagement',
    'Synchronization',
    'Maintenance'
  );
  FTerms.Add('Actualizar', Term);

  // ============================================================
  // Category: ACCUMULATIONS & TOTALS
  // ============================================================

  // Acumulado
  Term.SpanishTerm := 'Acumulado';
  Term.EnglishTerms := TArray<string>.Create(
    'Accumulated',
    'Total',
    'CumulativeTotal',
    'RunningTotal',
    'AggregateTotal',
    'SumTotal'
  );
  Term.Context := 'Accumulated or cumulative total. Used to track total costs and quantities accumulated across multiple work reports for a project section and reference combination.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'JobCosting',
    'Financial',
    'Aggregation'
  );
  FTerms.Add('Acumulado', Term);

  // Acumular
  Term.SpanishTerm := 'Acumular';
  Term.EnglishTerms := TArray<string>.Create(
    'Accumulate',
    'Aggregate',
    'Summarize',
    'Total',
    'RollUp'
  );
  Term.Context := 'Action of accumulating or rolling up transaction amounts to parent accounts or summary levels. Used for hierarchical account totaling.';
  Term.DomainTags := TArray<string>.Create(
    'AccountingProcess',
    'Calculation',
    'FinancialReporting'
  );
  FTerms.Add('Acumular', Term);

  // Vuelo (singular)
  Term.SpanishTerm := 'Vuelo';
  Term.EnglishTerms := TArray<string>.Create(
    'Flight',
    'FlightOperation',
    'FlightMovement',
    'AircraftOperation'
  );
  Term.Context := 'Individual flight operation with arrival/departure times, aircraft, crew, passengers, and handling services.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'FlightOperations',
    'FBOoperations'
  );
  FTerms.Add('Vuelo', Term);

  // Aeropuertos
  Term.SpanishTerm := 'Aeropuertos';
  Term.EnglishTerms := TArray<string>.Create(
    'Airports',
    'Aerodromes',
    'AirportFacilities',
    'AviationFacilities'
  );
  Term.Context := 'Airport master data including IATA/ICAO codes, locations, contact information, and operational parameters. Essential for flight planning and coordination.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'MasterData',
    'Infrastructure',
    'FlightOperations'
  );
  FTerms.Add('Aeropuertos', Term);

  // Agente
  Term.SpanishTerm := 'Agente';
  Term.EnglishTerms := TArray<string>.Create(
    'Agent',
    'SalesAgent',
    'SalesRepresentative',
    'SalesRep'
  );
  Term.Context := 'Sales agent or representative with commission management and sales tracking.';
  Term.DomainTags := TArray<string>.Create(
    'SalesManagement',
    'CommissionManagement',
    'SalesTeam',
    'CRM'
  );
  FTerms.Add('Agente', Term);

  // Agentes
  Term.SpanishTerm := 'Agentes';
  Term.EnglishTerms := TArray<string>.Create(
    'Agents',
    'SalesAgents',
    'SalesRepresentatives',
    'SalesReps',
    'GroundHandlingAgents',
    'HandlingAgents'
  );
  Term.Context := 'Plural form. Multiple sales agents.';
  Term.DomainTags := TArray<string>.Create(
    'SalesManagement',
    'CommissionManagement',
    'SalesTeam',
    'CRM'
  );
  FTerms.Add('Agentes', Term);

  // Agrupacion
  Term.SpanishTerm := 'Agrupacion';
  Term.EnglishTerms := TArray<string>.Create(
    'Grouping',
    'Batch',
    'Cluster',
    'OrderGroup'
  );
  Term.Context := 'Production order grouping';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'ProductionPlanning',
    'Optimization'
  );
  FTerms.Add('Agrupacion', Term);

  // Agrupar
  Term.SpanishTerm := 'Agrupar';
  Term.EnglishTerms := TArray<string>.Create(
    'Group',
    'Batch',
    'Cluster',
    'Aggregate',
    'Bundle'
  );
  Term.Context := 'Group production orders for batch processing and optimization';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'ProductionPlanning',
    'Optimization',
    'BatchProcessing'
  );
  FTerms.Add('Agrupar', Term);

  // Albaranar
  Term.SpanishTerm := 'Albaranar';
  Term.EnglishTerms := TArray<string>.Create(
    'CreateDeliveryNote',
    'Ship',
    'IssueDeliveryNote',
    'GenerateDeliveryNote'
  );
  Term.Context := 'Action of creating delivery notes from orders for shipment documentation. Business process step before invoicing.';
  Term.DomainTags := TArray<string>.Create(
    'Logistics',
    'SalesOperations',
    'ShippingManagement',
    'BusinessProcesses'
  );
  FTerms.Add('Albaranar', Term);

  // Albaranes
  Term.SpanishTerm := 'Albaranes';
  Term.EnglishTerms := TArray<string>.Create(
    'DeliveryNotes',
    'ShippingDocuments',
    'PackingSlips',
    'DeliverySlips',
    'GoodsReceiptNotes',
    'DespatchNotes'
  );
  Term.Context := 'Delivery notes documenting shipment of goods/services before invoicing. Pre-invoice document in Spanish business practice.';
  Term.DomainTags := TArray<string>.Create(
    'Logistics',
    'SalesManagement',
    'ShippingManagement',
    'DocumentManagement'
  );
  FTerms.Add('Albaranes', Term);

  // Albarán
  Term.SpanishTerm := 'Albarán';
  Term.EnglishTerms := TArray<string>.Create(
    'DeliveryNote',
    'PackingSlip',
    'ShippingDocument',
    'DispatchNote'
  );
  Term.Context := 'Delivery note document accompanying shipped goods. Spanish commercial document required for goods shipment.';
  Term.DomainTags := TArray<string>.Create(
    'ShippingManagement',
    'Logistics',
    'WarehouseManagement',
    'SalesOrders'
  );
  FTerms.Add('Albarán', Term);

  // Alias
  Term.SpanishTerm := 'Alias';
  Term.EnglishTerms := TArray<string>.Create(
    'AccountAlias',
    'Alias',
    'AlternateCode',
    'Shortcut',
    'NickName'
  );
  Term.Context := 'Alternative short code or alias for an account. Allows using memorable shortcuts instead of full account codes.';
  Term.DomainTags := TArray<string>.Create(
    'ChartOfAccounts',
    'AccountingConfiguration',
    'UserInterface'
  );
  FTerms.Add('Alias', Term);

  // ============================================================
  // ALMACEN MODULE GLOSSARY
  // ============================================================
  // ============================================================
  // Category: CORE ENTITIES & TABLES
  // ============================================================

  // Almacen
  Term.SpanishTerm := 'Almacen';
  Term.EnglishTerms := TArray<string>.Create(
    'Warehouse',
    'Location',
    'StorageLocation',
    'StockLocation',
    'WarehouseLocation',
    'Depot'
  );
  Term.Context := 'Physical warehouse location for inventory segmentation. Used as primary dimension for multi-warehouse inventory tracking.';
  Term.DomainTags := TArray<string>.Create(
    'WarehouseManagement',
    'InventoryTracking',
    'StockControl',
    'LocationManagement'
  );
  FTerms.Add('Almacen', Term);

  // Almacenes
  Term.SpanishTerm := 'Almacenes';
  Term.EnglishTerms := TArray<string>.Create(
    'Warehouses',
    'Stocks',
    'Inventories',
    'Depots',
    'Locations',
    'StorageLocations'
  );
  Term.Context := 'Plural form. Multiple warehouse locations.';
  Term.DomainTags := TArray<string>.Create(
    'WarehouseManagement',
    'Inventory',
    'StockControl',
    'Logistics'
  );
  FTerms.Add('Almacenes', Term);

  // Almacén
  Term.SpanishTerm := 'Almacén';
  Term.EnglishTerms := TArray<string>.Create(
    'Warehouse',
    'Stock',
    'Inventory',
    'Depot'
  );
  Term.Context := 'Warehouse location for inventory storage and stock management. Multi-warehouse inventory tracking support.';
  Term.DomainTags := TArray<string>.Create(
    'WarehouseManagement',
    'Inventory',
    'StockControl',
    'Logistics'
  );
  FTerms.Add('Almacén', Term);

  // Alquiler/Alquileres
  Term.SpanishTerm := 'Alquiler';
  Term.EnglishTerms := TArray<string>.Create(
    'Rental',
    'CarRental',
    'VehicleRental',
    'CrewCarRental',
    'GroundTransportation'
  );
  Term.Context := 'Car rental services for crew members including vehicle type, rental period, and billing. Part of comprehensive crew service coordination.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'CrewServices',
    'Transportation',
    'ServiceCoordination'
  );
  FTerms.Add('Alquiler', Term);

  // Amortización
  Term.SpanishTerm := 'Amortización';
  Term.EnglishTerms := TArray<string>.Create(
    'Depreciation',
    'Amortization',
    'AssetDepreciation',
    'WriteDown',
    'Depletion'
  );
  Term.Context := 'Depreciation or amortization of fixed assets. Calculated automatically and posted periodically to accounting.';
  Term.DomainTags := TArray<string>.Create(
    'FixedAssets',
    'Depreciation',
    'AssetManagement',
    'Accounting'
  );
  FTerms.Add('Amortización', Term);

  // Anomalia
  Term.SpanishTerm := 'Anomalia';
  Term.EnglishTerms := TArray<string>.Create(
    'Anomaly',
    'Issue',
    'Problem',
    'Error',
    'Exception'
  );
  Term.Context := 'Production phase has encountered an error or quality issue';
  Term.DomainTags := TArray<string>.Create(
    'ProcessControl',
    'ExceptionHandling',
    'QualityControl'
  );
  FTerms.Add('Anomalia', Term);

  // Anunciantes
  Term.SpanishTerm := 'Anunciantes';
  Term.EnglishTerms := TArray<string>.Create(
    'Advertisers',
    'Clients',
    'Brands',
    'Sponsors',
    'Customers'
  );
  Term.Context := 'Advertiser/client companies booking billboard space for advertising campaigns. Primary customer entity in billboard management system.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'CustomerManagement',
    'CRM',
    'SalesManagement'
  );
  FTerms.Add('Anunciantes', Term);

  // Apunte
  Term.SpanishTerm := 'Apunte';
  Term.EnglishTerms := TArray<string>.Create(
    'JournalLine',
    'EntryLine',
    'LedgerLine',
    'PostingLine',
    'TransactionLine'
  );
  Term.Context := 'Individual line item within a journal entry (asiento). Each apunte posts to one account with debit or credit amount.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'JournalEntries',
    'DoubleEntry',
    'Accounting'
  );
  FTerms.Add('Apunte', Term);

  // Apuntes
  Term.SpanishTerm := 'Apuntes';
  Term.EnglishTerms := TArray<string>.Create(
    'JournalLines',
    'EntryLines',
    'LedgerLines',
    'PostingLines',
    'TransactionLines'
  );
  Term.Context := 'Multiple line items within journal entries. The detailed transactions that make up accounting entries.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'JournalEntries',
    'Accounting'
  );
  FTerms.Add('Apuntes', Term);

  // Archivar
  Term.SpanishTerm := 'Archivar';
  Term.EnglishTerms := TArray<string>.Create(
    'Archive',
    'Store',
    'FileAway',
    'PreserveDocuments',
    'DocumentArchiving'
  );
  Term.Context := 'Action of archiving documents (PDFs, files) for long-term storage and regulatory compliance. Essential for document lifecycle management.';
  Term.DomainTags := TArray<string>.Create(
    'DocumentManagement',
    'Archiving',
    'Compliance',
    'RecordsManagement'
  );
  FTerms.Add('Archivar', Term);

  // ArtiLins
  Term.SpanishTerm := 'ArtiLins';
  Term.EnglishTerms := TArray<string>.Create(
    'ArticleLines',
    'ItemLines',
    'ProductLines',
    'RouteDetails'
  );
  Term.Context := 'Article route line details for manufacturing process definition';
  Term.DomainTags := TArray<string>.Create(
    'ProcessPlanning',
    'Routing',
    'Manufacturing'
  );
  FTerms.Add('ArtiLins', Term);

  // Articulo
  Term.SpanishTerm := 'Articulo';
  Term.EnglishTerms := TArray<string>.Create(
    'Item',
    'Article',
    'Product',
    'SKU',
    'Part',
    'StockItem'
  );
  Term.Context := 'Individual product item';
  Term.DomainTags := TArray<string>.Create(
    'ProductManagement',
    'Inventory',
    'Catalog',
    'InventoryTracking'
  );
  FTerms.Add('Articulo', Term);

  // Articulos
  Term.SpanishTerm := 'Articulos';
  Term.EnglishTerms := TArray<string>.Create(
    'Items',
    'Articles',
    'Products',
    'SKUs',
    'Parts',
    'StockItems'
  );
  Term.Context := 'Product items and articles in inventory and production';
  Term.DomainTags := TArray<string>.Create(
    'ProductManagement',
    'Inventory',
    'Catalog',
    'Manufacturing'
  );
  FTerms.Add('Articulos', Term);

  // Artículo
  Term.SpanishTerm := 'Artículo';
  Term.EnglishTerms := TArray<string>.Create(
    'Article',
    'Product',
    'Item',
    'SKU'
  );
  Term.Context := 'Product or article master record with variants, pricing, and inventory tracking.';
  Term.DomainTags := TArray<string>.Create(
    'ProductManagement',
    'Inventory',
    'CatalogManagement',
    'StockControl'
  );
  FTerms.Add('Artículo', Term);

  // Artículos
  Term.SpanishTerm := 'Artículos';
  Term.EnglishTerms := TArray<string>.Create(
    'Articles',
    'Products',
    'Items',
    'SKUs'
  );
  Term.Context := 'Plural form. Product catalog entries.';
  Term.DomainTags := TArray<string>.Create(
    'ProductManagement',
    'Inventory',
    'CatalogManagement',
    'StockControl'
  );
  FTerms.Add('Artículos', Term);

  // ============================================================
  // TCCONTA MODULE GLOSSARY
  // ============================================================
  // ============================================================
  // Category: JOURNAL ENTRIES & POSTING
  // ============================================================

  // Asiento
  Term.SpanishTerm := 'Asiento';
  Term.EnglishTerms := TArray<string>.Create(
    'JournalEntry',
    'AccountingEntry',
    'BookEntry',
    'LedgerEntry',
    'JournalTransaction',
    'BookEntry'
  );
  Term.Context := 'Accounting journal entry containing one or more line items (apuntes) that must balance. The fundamental unit of double-entry bookkeeping in Spanish accounting.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'JournalEntries',
    'DoubleEntry',
    'Accounting'
  );
  FTerms.Add('Asiento', Term);

  // Asientos
  Term.SpanishTerm := 'Asientos';
  Term.EnglishTerms := TArray<string>.Create(
    'JournalEntries',
    'AccountingEntries',
    'BookEntries',
    'LedgerEntries',
    'Transactions',
    'BookEntries'
  );
  Term.Context := 'Multiple accounting journal entries. Used when managing or listing several transactions.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'JournalEntries',
    'Accounting',
    'Accounting'
  );
  FTerms.Add('Asientos', Term);

  // Asistente
  Term.SpanishTerm := 'Asistente';
  Term.EnglishTerms := TArray<string>.Create(
    'Wizard',
    'Assistant',
    'Helper',
    'Guide',
    'StepByStep'
  );
  Term.Context := 'Wizard or step-by-step assistant for complex operations. Guides users through multi-step processes like template creation or batch posting.';
  Term.DomainTags := TArray<string>.Create(
    'UserInterface',
    'Productivity',
    'Wizards',
    'Automation'
  );
  FTerms.Add('Asistente', Term);

  // ============================================================
  // Category: PHASE STATES
  // ============================================================

  // Ausente
  Term.SpanishTerm := 'Ausente';
  Term.EnglishTerms := TArray<string>.Create(
    'Absent',
    'NotPresent',
    'NotConfigured',
    'NotApplicable'
  );
  Term.Context := 'Production phase not configured for this order (9-phase system)';
  Term.DomainTags := TArray<string>.Create(
    'ProcessControl',
    'PhaseManagement',
    'Manufacturing'
  );
  FTerms.Add('Ausente', Term);

  // Autorizada
  Term.SpanishTerm := 'Autorizada';
  Term.EnglishTerms := TArray<string>.Create(
    'Authorized',
    'Approved',
    'Validated',
    'Confirmed'
  );
  Term.Context := 'Production order authorized and ready for planning';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'OrderStates',
    'WorkflowApproval'
  );
  FTerms.Add('Autorizada', Term);

  // Avanzar
  Term.SpanishTerm := 'Avanzar';
  Term.EnglishTerms := TArray<string>.Create(
    'Advance',
    'Progress',
    'MoveForward',
    'Next'
  );
  Term.Context := 'Advance progress indicator or move to next step. Used in progress dialogs and wizards.';
  Term.DomainTags := TArray<string>.Create(
    'ProgressTracking',
    'UserInterface',
    'Navigation'
  );
  FTerms.Add('Avanzar', Term);

  // ============================================================
  // Category: REPORTING & ANALYSIS
  // ============================================================

  // Balance
  Term.SpanishTerm := 'Balance';
  Term.EnglishTerms := TArray<string>.Create(
    'BalanceSheet',
    'Balance',
    'FinancialStatement',
    'TrialBalance',
    'AccountBalance'
  );
  Term.Context := 'Balance sheet or trial balance financial statement. Shows assets, liabilities, and equity at specific date.';
  Term.DomainTags := TArray<string>.Create(
    'FinancialReporting',
    'Reporting',
    'Accounting',
    'Analysis'
  );
  FTerms.Add('Balance', Term);

  // Borrar
  Term.SpanishTerm := 'Borrar';
  Term.EnglishTerms := TArray<string>.Create(
    'Delete',
    'Remove',
    'Erase',
    'Destroy'
  );
  Term.Context := 'Delete operation for records or files. Removes entities from database or file system.';
  Term.DomainTags := TArray<string>.Create(
    'DataManagement',
    'CRUD',
    'RecordDeletion'
  );
  FTerms.Add('Borrar', Term);

  // Buscar
  Term.SpanishTerm := 'Buscar';
  Term.EnglishTerms := TArray<string>.Create(
    'Search',
    'Find',
    'Lookup',
    'Query'
  );
  Term.Context := 'Search functionality for finding records in database tables. Includes quick search, advanced search, and filter-based searching.';
  Term.DomainTags := TArray<string>.Create(
    'Search',
    'DataRetrieval',
    'UserInterface',
    'QueryBuilder'
  );
  FTerms.Add('Buscar', Term);

  // COSTE
  Term.SpanishTerm := 'COSTE';
  Term.EnglishTerms := TArray<string>.Create(
    'Cost',
    'ProductCost',
    'ItemCost',
    'UnitCost',
    'CostPrice',
    'PurchaseCost'
  );
  Term.Context := 'Product cost data class. Standard cost for inventory valuation and margin calculation. Non-incrementable (=) class.';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'InventoryValuation',
    'ProductCosting',
    'FinancialReporting'
  );
  FTerms.Add('COSTE', Term);

  // COSTENETO
  Term.SpanishTerm := 'COSTENETO';
  Term.EnglishTerms := TArray<string>.Create(
    'NetCost',
    'NetProductCost',
    'NetPurchaseCost',
    'CostAfterDiscounts',
    'ActualCost',
    'TrueCost'
  );
  Term.Context := 'Net product cost after discounts and adjustments. Used for accurate profitability analysis. Non-incrementable (=) class.';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'InventoryValuation',
    'ProductCosting',
    'ProfitabilityAnalysis'
  );
  FTerms.Add('COSTENETO', Term);

  // CSB
  Term.SpanishTerm := 'CSB';
  Term.EnglishTerms := TArray<string>.Create(
    'SpanishBankingNorms',
    'CSBNorms',
    'BankingStandards',
    'NormaCSB',
    'SpanishBankFormat'
  );
  Term.Context := 'Consejo Superior Bancario - Legacy Spanish banking file formats (Norma 19, 32, 58, 68). Being replaced by SEPA standards.';
  Term.DomainTags := TArray<string>.Create(
    'BankingOperations',
    'PaymentProcessing',
    'SpanishStandards',
    'Legacy'
  );
  FTerms.Add('CSB', Term);

  // Cabecera
  Term.SpanishTerm := 'Cabecera';
  Term.EnglishTerms := TArray<string>.Create(
    'Header',
    'DocumentHeader',
    'OrderHeader',
    'InvoiceHeader'
  );
  Term.Context := 'Document header containing master information for invoices, orders, quotes, delivery notes. Main entity for all commercial documents.';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'Invoicing',
    'SalesOrders',
    'DocumentManagement'
  );
  FTerms.Add('Cabecera', Term);

  // Cabeceras
  Term.SpanishTerm := 'Cabeceras';
  Term.EnglishTerms := TArray<string>.Create(
    'Headers',
    'DocumentHeaders',
    'OrderHeaders',
    'InvoiceHeaders'
  );
  Term.Context := 'Plural form. Multiple document headers in the system.';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'Invoicing',
    'SalesOrders',
    'DocumentManagement'
  );
  FTerms.Add('Cabeceras', Term);

  // Caducidad
  Term.SpanishTerm := 'Caducidad';
  Term.EnglishTerms := TArray<string>.Create(
    'ExpirationDate',
    'Expiry',
    'BestBefore',
    'UseByDate',
    'ShelfLife',
    'ExpiryDate'
  );
  Term.Context := 'Product expiration or best-before date. Segmentation dimension for perishable goods and FEFO (First-Expired-First-Out) inventory management.';
  Term.DomainTags := TArray<string>.Create(
    'InventoryTracking',
    'QualityControl',
    'PerishableGoods',
    'DateTracking'
  );
  FTerms.Add('Caducidad', Term);

  // ============================================================
  // Category: CASH MANAGEMENT
  // ============================================================

  // Caja
  Term.SpanishTerm := 'Caja';
  Term.EnglishTerms := TArray<string>.Create(
    'Cash',
    'CashRegister',
    'PettyCash',
    'CashBox',
    'CashAccount'
  );
  Term.Context := 'Cash register or petty cash management module. Records cash transactions before posting to accounting ledger.';
  Term.DomainTags := TArray<string>.Create(
    'CashManagement',
    'Treasury',
    'Accounting'
  );
  FTerms.Add('Caja', Term);

  // Calcular
  Term.SpanishTerm := 'Calcular';
  Term.EnglishTerms := TArray<string>.Create(
    'Calculate',
    'Compute',
    'DetermineValue',
    'ProcessCalculation',
    'CalculateTotal',
    'Determine'
  );
  Term.Context := 'Action of performing calculations for pricing, totals, taxes, costs, or other business computations.';
  Term.DomainTags := TArray<string>.Create(
    'Calculation',
    'BusinessLogic',
    'Pricing',
    'FinancialProcessing'
  );
  FTerms.Add('Calcular', Term);

  // Cancelada
  Term.SpanishTerm := 'Cancelada';
  Term.EnglishTerms := TArray<string>.Create(
    'Cancelled',
    'Aborted',
    'Voided',
    'Terminated'
  );
  Term.Context := 'Production order cancelled and will not be executed';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'OrderStates',
    'ExceptionHandling'
  );
  FTerms.Add('Cancelada', Term);

  // Cargados
  Term.SpanishTerm := 'Cargados';
  Term.EnglishTerms := TArray<string>.Create(
    'Loaded',
    'LoadedData',
    'Retrieved'
  );
  Term.Context := 'Loaded state indicator. Shows that data has been successfully loaded into memory.';
  Term.DomainTags := TArray<string>.Create(
    'DataManagement',
    'StateManagement'
  );
  FTerms.Add('Cargados', Term);

  // ============================================================
  // Category: DATA ACTIONS & VERBS
  // ============================================================

  // Cargar
  Term.SpanishTerm := 'Cargar';
  Term.EnglishTerms := TArray<string>.Create(
    'Load',
    'LoadData',
    'Read',
    'Retrieve'
  );
  Term.Context := 'Load operation for data, files, or configurations. Reads data from storage into memory.';
  Term.DomainTags := TArray<string>.Create(
    'DataManagement',
    'FileManagement',
    'ConfigurationManagement'
  );
  FTerms.Add('Cargar', Term);

  // CategoriasAviones
  Term.SpanishTerm := 'CategoriasAviones';
  Term.EnglishTerms := TArray<string>.Create(
    'AircraftCategories',
    'WeightCategories',
    'AircraftClassification',
    'PricingCategories'
  );
  Term.Context := 'Aircraft weight-based categories for pricing and service classification. Essential for dynamic pricing calculation.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'Pricing',
    'Classification',
    'FleetManagement'
  );
  FTerms.Add('CategoriasAviones', Term);

  // ============================================================
  // Category: COST CENTERS
  // ============================================================

  // Centro
  Term.SpanishTerm := 'Centro';
  Term.EnglishTerms := TArray<string>.Create(
    'CostCenter',
    'Center',
    'ProfitCenter',
    'Department',
    'CostUnit'
  );
  Term.Context := 'Cost center or profit center for analytical accounting. Optional dimension tracked alongside account codes in journal entries.';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'AnalyticalAccounting',
    'Dimensions',
    'Management'
  );
  FTerms.Add('Centro', Term);

  // Centros
  Term.SpanishTerm := 'Centros';
  Term.EnglishTerms := TArray<string>.Create(
    'CostCenters',
    'Centers',
    'ProfitCenters',
    'Departments',
    'CostUnits'
  );
  Term.Context := 'Multiple cost centers for analytical accounting. Hierarchical structure similar to chart of accounts.';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'AnalyticalAccounting',
    'Dimensions'
  );
  FTerms.Add('Centros', Term);

  // Cerrada
  Term.SpanishTerm := 'Cerrada';
  Term.EnglishTerms := TArray<string>.Create(
    'Closed',
    'Completed',
    'Finalized',
    'Archived'
  );
  Term.Context := 'Production order closed after being finished, no further modifications allowed';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'OrderStates',
    'OrderCompletion'
  );
  FTerms.Add('Cerrada', Term);

  // Cierre
  Term.SpanishTerm := 'Cierre';
  Term.EnglishTerms := TArray<string>.Create(
    'YearEndClosing',
    'Closing',
    'PeriodClose',
    'FiscalClose',
    'AccountingClose'
  );
  Term.Context := 'Fiscal year-end closing process. Transfers balances to next year and creates closing/opening journal entries.';
  Term.DomainTags := TArray<string>.Create(
    'FiscalYear',
    'YearEndProcess',
    'Accounting',
    'FinancialReporting'
  );
  FTerms.Add('Cierre', Term);

  // Circuitos
  Term.SpanishTerm := 'Circuitos';
  Term.EnglishTerms := TArray<string>.Create(
    'Circuits',
    'BillboardCircuits',
    'Routes',
    'MediaPackages',
    'BillboardNetworks'
  );
  Term.Context := 'Billboard circuits grouping multiple billboards into packages for campaign planning and sales. Enables bundled pricing and network campaigns.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'ProductManagement',
    'PackageManagement',
    'SalesOperations'
  );
  FTerms.Add('Circuitos', Term);

  // Circular
  Term.SpanishTerm := 'Circular';
  Term.EnglishTerms := TArray<string>.Create(
    'Circular',
    'Newsletter',
    'BulkCommunication',
    'MassMailing'
  );
  Term.Context := 'Mass communication or newsletter to customers. Bulk email or document distribution to customer segments.';
  Term.DomainTags := TArray<string>.Create(
    'Marketing',
    'CustomerCommunication',
    'EmailMarketing',
    'BulkMessaging'
  );
  FTerms.Add('Circular', Term);

  // Circulares
  Term.SpanishTerm := 'Circulares';
  Term.EnglishTerms := TArray<string>.Create(
    'Circulars',
    'Newsletters',
    'BulkCommunications',
    'MassMailings'
  );
  Term.Context := 'Plural form. Multiple marketing communications and campaigns.';
  Term.DomainTags := TArray<string>.Create(
    'Marketing',
    'CustomerCommunication',
    'EmailMarketing',
    'BulkMessaging'
  );
  FTerms.Add('Circulares', Term);

  // ============================================================
  // Category: CLASSIFICATION
  // ============================================================

  // Clase
  Term.SpanishTerm := 'Clase';
  Term.EnglishTerms := TArray<string>.Create(
    'Class',
    'Category',
    'Type',
    'Classification',
    'WorkClass',
    'JobClass'
  );
  Term.Context := 'A class or category for work reports. Each parte has a class (MATE, TRAB, or GAS) that determines its type and how costs are calculated. Also used to categorize workers and other entities.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'Classification',
    'DataOrganization',
    'BusinessEntity'
  );
  FTerms.Add('Clase', Term);

  // ClaseDato
  Term.SpanishTerm := 'ClaseDato';
  Term.EnglishTerms := TArray<string>.Create(
    'DataClass',
    'ValueType',
    'MetricType',
    'DataCategory',
    'ClassificationType',
    'DataType'
  );
  Term.Context := 'Type of product data being tracked (STOCK, PVP, COSTE, etc.). Runtime-configurable data model allowing custom metrics without recompilation.';
  Term.DomainTags := TArray<string>.Create(
    'DataModeling',
    'EventSourcing',
    'ConfigurableSchema',
    'MetadataManagement'
  );
  FTerms.Add('ClaseDato', Term);

  // Clases
  Term.SpanishTerm := 'Clases';
  Term.EnglishTerms := TArray<string>.Create(
    'Classes',
    'Categories',
    'Types',
    'Classifications',
    'WorkClasses'
  );
  Term.Context := 'Multiple classes or categories. Used to organize and classify different types of work reports and other entities in the system.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'Classification',
    'DataOrganization',
    'BusinessEntity'
  );
  FTerms.Add('Clases', Term);

  // ClaveSegmentos
  Term.SpanishTerm := 'ClaveSegmentos';
  Term.EnglishTerms := TArray<string>.Create(
    'SegmentsKey',
    'DimensionKey',
    'CompositeKey',
    'SegmentationKey',
    'MultiDimensionalKey',
    'CombinedSegments'
  );
  Term.Context := 'Auto-calculated composite key combining relevant segments (format: ALMACEN|TALLA|COLOR|LOTE|...). Used for grouping related events in event sourcing system.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'DataModeling',
    'Segmentation',
    'KeyManagement'
  );
  FTerms.Add('ClaveSegmentos', Term);

  // ============================================================
  // Category: BUSINESS ENTITIES
  // ============================================================

  // Cliente
  Term.SpanishTerm := 'Cliente';
  Term.EnglishTerms := TArray<string>.Create(
    'Customer',
    'Client',
    'Account',
    'Buyer',
    'ClientApplication',
    'ClientWorkstation'
  );
  Term.Context := 'Customer entity with credit control, pricing rules, and commercial relationships. Primary sales counterpart.';
  Term.DomainTags := TArray<string>.Create(
    'CustomerManagement',
    'CRM',
    'SalesOrders',
    'AccountsReceivable'
  );
  FTerms.Add('Cliente', Term);

  // Clientes
  Term.SpanishTerm := 'Clientes';
  Term.EnglishTerms := TArray<string>.Create(
    'Customers',
    'Clients',
    'Accounts',
    'Buyers',
    'CustomerAccounts'
  );
  Term.Context := 'Plural form. Multiple customer entities.';
  Term.DomainTags := TArray<string>.Create(
    'CustomerManagement',
    'CRM',
    'SalesOrders',
    'AccountsReceivable'
  );
  FTerms.Add('Clientes', Term);

  // Cobro
  Term.SpanishTerm := 'Cobro';
  Term.EnglishTerms := TArray<string>.Create(
    'Collection',
    'Receipt',
    'Payment',
    'CashReceipt'
  );
  Term.Context := 'Payment collection or receipt from customer. Cash or bank receipt transaction.';
  Term.DomainTags := TArray<string>.Create(
    'AccountsReceivable',
    'CashManagement',
    'PaymentProcessing',
    'Receipts'
  );
  FTerms.Add('Cobro', Term);

  // Cobros
  Term.SpanishTerm := 'Cobros';
  Term.EnglishTerms := TArray<string>.Create(
    'Collections',
    'Receipts',
    'Payments',
    'CashReceipts'
  );
  Term.Context := 'Plural form. Multiple payment collections and receipts.';
  Term.DomainTags := TArray<string>.Create(
    'AccountsReceivable',
    'CashManagement',
    'PaymentProcessing',
    'Receipts'
  );
  FTerms.Add('Cobros', Term);

  // Color
  Term.SpanishTerm := 'Color';
  Term.EnglishTerms := TArray<string>.Create(
    'Color',
    'Colour',
    'ProductColor',
    'ColorVariant',
    'ColorVariant',
    'ColorDimension'
  );
  Term.Context := 'Product color variant dimension. Used extensively in fashion retail for color/size matrix management.';
  Term.DomainTags := TArray<string>.Create(
    'ProductVariants',
    'FashionRetail',
    'ProductAttributes',
    'VariantManagement'
  );
  FTerms.Add('Color', Term);

  // Colores
  Term.SpanishTerm := 'Colores';
  Term.EnglishTerms := TArray<string>.Create(
    'Colors',
    'Colours',
    'ProductColors',
    'ColorVariants'
  );
  Term.Context := 'Plural form. Color variant catalog.';
  Term.DomainTags := TArray<string>.Create(
    'ProductVariants',
    'FashionRetail',
    'ProductAttributes',
    'VariantManagement'
  );
  FTerms.Add('Colores', Term);

  // Comisiones
  Term.SpanishTerm := 'Comisiones';
  Term.EnglishTerms := TArray<string>.Create(
    'Commissions',
    'SalesCommissions',
    'AgentCommissions',
    'CommissionRates'
  );
  Term.Context := 'Plural form. Commission structures and calculations.';
  Term.DomainTags := TArray<string>.Create(
    'CommissionManagement',
    'SalesManagement',
    'AgentCompensation',
    'Incentives'
  );
  FTerms.Add('Comisiones', Term);

  // Comisión
  Term.SpanishTerm := 'Comisión';
  Term.EnglishTerms := TArray<string>.Create(
    'Commission',
    'SalesCommission',
    'AgentCommission',
    'CommissionRate'
  );
  Term.Context := 'Sales commission for agents. Complex commission calculation system with advanced rules.';
  Term.DomainTags := TArray<string>.Create(
    'CommissionManagement',
    'SalesManagement',
    'AgentCompensation',
    'Incentives'
  );
  FTerms.Add('Comisión', Term);

  // CompArti
  Term.SpanishTerm := 'CompArti';
  Term.EnglishTerms := TArray<string>.Create(
    'ArticleComponents',
    'ItemComponents',
    'ProductComponents',
    'BOMItems'
  );
  Term.Context := 'Article component records in bill of materials';
  Term.DomainTags := TArray<string>.Create(
    'BillOfMaterials',
    'ProductStructure',
    'Inventory'
  );
  FTerms.Add('CompArti', Term);

  // Compañías
  Term.SpanishTerm := 'Compañías';
  Term.EnglishTerms := TArray<string>.Create(
    'Airlines',
    'Companies',
    'OperatingCompanies',
    'AirOperators',
    'Carriers'
  );
  Term.Context := 'Airlines and operating companies with SITA addresses, service charges, and billing configurations. Key customer entities.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'CustomerManagement',
    'MasterData',
    'Billing'
  );
  FTerms.Add('Compañías', Term);

  // Componentes
  Term.SpanishTerm := 'Componentes';
  Term.EnglishTerms := TArray<string>.Create(
    'Components',
    'Parts',
    'Materials',
    'Ingredients'
  );
  Term.Context := 'Components and parts used in product assembly';
  Term.DomainTags := TArray<string>.Create(
    'BillOfMaterials',
    'Inventory',
    'Manufacturing'
  );
  FTerms.Add('Componentes', Term);

  // Composicion
  Term.SpanishTerm := 'Composicion';
  Term.EnglishTerms := TArray<string>.Create(
    'Composition',
    'BillOfMaterials',
    'BOM',
    'MaterialList',
    'Components'
  );
  Term.Context := 'Bill of materials defining product composition and material requirements';
  Term.DomainTags := TArray<string>.Create(
    'BillOfMaterials',
    'MaterialPlanning',
    'ProductStructure',
    'Manufacturing'
  );
  FTerms.Add('Composicion', Term);

  // Proveedor (singular)
  Term.SpanishTerm := 'Proveedor';
  Term.EnglishTerms := TArray<string>.Create(
    'Supplier',
    'Provider',
    'Vendor',
    'ServiceProvider'
  );
  Term.Context := 'Individual supplier/provider account for procurement and service coordination.';
  Term.DomainTags := TArray<string>.Create(
    'SupplierManagement',
    'Procurement',
    'VendorManagement'
  );
  FTerms.Add('Proveedor', Term);

  // Conceptos
  Term.SpanishTerm := 'Conceptos';
  Term.EnglishTerms := TArray<string>.Create(
    'Concepts',
    'Items',
    'ServiceConcepts',
    'ProductConcepts',
    'LineItems'
  );
  Term.Context := 'Service or product concept definitions used as billable items in invoices and orders. Catalog of billable services/products.';
  Term.DomainTags := TArray<string>.Create(
    'ProductManagement',
    'ServiceManagement',
    'MasterData',
    'Catalog'
  );
  FTerms.Add('Conceptos', Term);

  // Conexion
  Term.SpanishTerm := 'Conexion';
  Term.EnglishTerms := TArray<string>.Create(
    'Connection',
    'NetworkConnection',
    'ServerConnection',
    'DatabaseConnection'
  );
  Term.Context := 'Network or database connection management. Handles establishing, maintaining, and closing connections.';
  Term.DomainTags := TArray<string>.Create(
    'NetworkCommunication',
    'DatabaseAccess',
    'ConnectionManagement'
  );
  FTerms.Add('Conexion', Term);

  // Contabiliza
  Term.SpanishTerm := 'Contabiliza';
  Term.EnglishTerms := TArray<string>.Create(
    'Posts',
    'Accounts',
    'Journals',
    'Records',
    'Books'
  );
  Term.Context := 'Third-person singular present tense: posts or records accounting transaction to ledger.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'AccountingProcess',
    'Posting'
  );
  FTerms.Add('Contabiliza', Term);

  // Contabilizado
  Term.SpanishTerm := 'Contabilizado';
  Term.EnglishTerms := TArray<string>.Create(
    'Posted',
    'Recorded',
    'Journalized',
    'Booked',
    'Accounted'
  );
  Term.Context := 'Status indicating a transaction has been posted to the accounting ledger. Cannot be modified without reversing.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'AccountingProcess',
    'Status',
    'Posting'
  );
  FTerms.Add('Contabilizado', Term);

  // Contabilizar
  Term.SpanishTerm := 'Contabilizar';
  Term.EnglishTerms := TArray<string>.Create(
    'PostToLedger',
    'AccountFor',
    'Journalize',
    'Record',
    'BookTransaction'
  );
  Term.Context := 'Action of posting transactions to the accounting ledger. Creates journal entries from source documents (invoices, cash, etc.).';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'JournalEntries',
    'AccountingProcess',
    'Posting'
  );
  FTerms.Add('Contabilizar', Term);

  // Contrataciones
  Term.SpanishTerm := 'Contrataciones';
  Term.EnglishTerms := TArray<string>.Create(
    'Bookings',
    'Contracts',
    'Reservations',
    'BillboardBookings',
    'MediaBookings',
    'CampaignContracts'
  );
  Term.Context := 'Billboard booking contracts linking advertisers to specific billboards for defined periods with pricing and campaign details. Core transactional entity.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'ContractManagement',
    'CampaignManagement',
    'SalesOperations'
  );
  FTerms.Add('Contrataciones', Term);

  // Contratos
  Term.SpanishTerm := 'Contratos';
  Term.EnglishTerms := TArray<string>.Create(
    'Contracts',
    'Agreements',
    'MediaContracts',
    'AdvertisingContracts'
  );
  Term.Context := 'Master contracts with advertisers defining terms, rates, and campaign parameters for billboard advertising services.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'ContractManagement',
    'LegalDocuments',
    'SalesManagement'
  );
  FTerms.Add('Contratos', Term);

  // ============================================================
  // VCL MODULE GLOSSARY
  // ============================================================
  // ============================================================
  // Category: APPLICATION COORDINATION & FRAMEWORK
  // ============================================================

  // ControladorAereo
  Term.SpanishTerm := 'ControladorAereo';
  Term.EnglishTerms := TArray<string>.Create(
    'AirTrafficController',
    'Coordinator',
    'ApplicationCoordinator',
    'CentralCoordinator',
    'SystemCoordinator'
  );
  Term.Context := 'Core application coordination system that manages licensing, server connections, and application state. Acts as central controller for multi-tier applications.';
  Term.DomainTags := TArray<string>.Create(
    'ApplicationFramework',
    'Coordination',
    'SystemManagement',
    'MultiTier'
  );
  FTerms.Add('ControladorAereo', Term);

  // Coordinacion
  Term.SpanishTerm := 'Coordinacion';
  Term.EnglishTerms := TArray<string>.Create(
    'Coordination',
    'Orchestration',
    'SystemCoordination',
    'ServiceCoordination'
  );
  Term.Context := 'Service coordination infrastructure for managing communication between application components and external services.';
  Term.DomainTags := TArray<string>.Create(
    'ApplicationFramework',
    'Coordination',
    'ServiceManagement'
  );
  FTerms.Add('Coordinacion', Term);

  // Coordinación
  Term.SpanishTerm := 'Coordinación';
  Term.EnglishTerms := TArray<string>.Create(
    'Coordination',
    'ServiceCoordination',
    'OperationalCoordination',
    'HandlingCoordination',
    'FlightCoordination'
  );
  Term.Context := 'Service coordination requests for flight operations including hotels, catering, parking, and ground services. Workflow orchestration for FBO operations.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'ServiceCoordination',
    'WorkflowManagement',
    'FBOoperations'
  );
  FTerms.Add('Coordinación', Term);

  // Coste
  Term.SpanishTerm := 'Coste';
  Term.EnglishTerms := TArray<string>.Create(
    'Cost',
    'UnitCost',
    'ProductCost',
    'ItemCost',
    'ItemCost',
    'ProjectCost'
  );
  Term.Context := 'Product cost for margin and profitability calculations. Tracked per warehouse and with cost history.';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'ProductCosting',
    'Inventory',
    'ProfitabilityAnalysis'
  );
  FTerms.Add('Coste', Term);

  // Costes
  Term.SpanishTerm := 'Costes';
  Term.EnglishTerms := TArray<string>.Create(
    'Costs',
    'UnitCosts',
    'ProductCosts',
    'ItemCosts',
    'ItemCosts',
    'ProjectCosts'
  );
  Term.Context := 'Plural form. Cost structures and costing data.';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'ProductCosting',
    'Inventory',
    'ProfitabilityAnalysis'
  );
  FTerms.Add('Costes', Term);

  // Crear
  Term.SpanishTerm := 'Crear';
  Term.EnglishTerms := TArray<string>.Create(
    'Create',
    'New',
    'Generate',
    'Make'
  );
  Term.Context := 'Create operation for new records, files, or objects. Instantiates new entities.';
  Term.DomainTags := TArray<string>.Create(
    'DataManagement',
    'CRUD',
    'ObjectCreation'
  );
  FTerms.Add('Crear', Term);

  // ============================================================
  // Category: CHART OF ACCOUNTS
  // ============================================================

  // Cuenta
  Term.SpanishTerm := 'Cuenta';
  Term.EnglishTerms := TArray<string>.Create(
    'Account',
    'GLAccount',
    'LedgerAccount',
    'ChartAccount',
    'GeneralLedgerAccount',
    'AccountingAccount'
  );
  Term.Context := 'General ledger account in the chart of accounts. Hierarchical account code with configurable levels (typically 3-5 levels).';
  Term.DomainTags := TArray<string>.Create(
    'ChartOfAccounts',
    'GeneralLedger',
    'Accounting',
    'Accounting'
  );
  FTerms.Add('Cuenta', Term);

  // Cuentas
  Term.SpanishTerm := 'Cuentas';
  Term.EnglishTerms := TArray<string>.Create(
    'Accounts',
    'GLAccounts',
    'LedgerAccounts',
    'ChartAccounts',
    'GeneralLedgerAccounts',
    'AccountingAccounts'
  );
  Term.Context := 'Multiple general ledger accounts. The complete chart of accounts or subset of accounts.';
  Term.DomainTags := TArray<string>.Create(
    'ChartOfAccounts',
    'GeneralLedger',
    'Accounting',
    'Accounting'
  );
  FTerms.Add('Cuentas', Term);

  // ============================================================
  // Category: COMMON FIELDS
  // ============================================================

  // Código
  Term.SpanishTerm := 'Código';
  Term.EnglishTerms := TArray<string>.Create(
    'Code',
    'ID',
    'Identifier',
    'Key'
  );
  Term.Context := 'Code or identifier field. Primary key or unique identifier for entities.';
  Term.DomainTags := TArray<string>.Create(
    'DataFields',
    'Identifiers',
    'MasterData',
    'KeyFields'
  );
  FTerms.Add('Código', Term);

  // DatArt
  Term.SpanishTerm := 'DatArt';
  Term.EnglishTerms := TArray<string>.Create(
    'ProductData',
    'ItemData',
    'MaterializedView',
    'CurrentValues',
    'DataCache',
    'PrecomputedData'
  );
  Term.Context := 'Materialized view table with pre-computed current product values (CQRS query side). Auto-updated from event log for fast queries.';
  Term.DomainTags := TArray<string>.Create(
    'CQRS',
    'EventSourcing',
    'PerformanceOptimization',
    'InventoryTracking'
  );
  FTerms.Add('DatArt', Term);

  // Datos
  Term.SpanishTerm := 'Datos';
  Term.EnglishTerms := TArray<string>.Create(
    'Data',
    'DatabaseData',
    'Information',
    'DataSet'
  );
  Term.Context := 'Application data, database data, or configuration data. Used throughout system for data management.';
  Term.DomainTags := TArray<string>.Create(
    'DatabaseAccess',
    'DataManagement',
    'Information'
  );
  FTerms.Add('Datos', Term);

  // DatosArticulos
  Term.SpanishTerm := 'DatosArticulos';
  Term.EnglishTerms := TArray<string>.Create(
    'ProductData',
    'ItemData',
    'ArticleData',
    'MaterializedView',
    'CurrentValuesTable',
    'ProductDataTable'
  );
  Term.Context := 'Full name of DatArt table. Stores pre-computed current values for all product data classes (STOCK, PVP, COSTE, etc.) for fast querying.';
  Term.DomainTags := TArray<string>.Create(
    'CQRS',
    'EventSourcing',
    'DatabaseSchema',
    'InventoryTracking'
  );
  FTerms.Add('DatosArticulos', Term);

  // Declaración
  Term.SpanishTerm := 'Declaración';
  Term.EnglishTerms := TArray<string>.Create(
    'TaxDeclaration',
    'TaxReturn',
    'Filing',
    'TaxStatement',
    'Declaration'
  );
  Term.Context := 'Official tax declaration or return filed with Spanish tax authority. Examples: Modelo 303 (VAT), Modelo 300 (annual VAT summary).';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'SpanishTax',
    'Filing',
    'Regulatory'
  );
  FTerms.Add('Declaración', Term);

  // Densidades
  Term.SpanishTerm := 'Densidades';
  Term.EnglishTerms := TArray<string>.Create(
    'Densities',
    'MaterialDensities',
    'SpecificGravity',
    'MaterialProperties'
  );
  Term.Context := 'Material density measurements for manufacturing quality control and product specifications. Essential for material validation and process control.';
  Term.DomainTags := TArray<string>.Create(
    'Manufacturing',
    'QualityControl',
    'MaterialManagement',
    'ProcessControl'
  );
  FTerms.Add('Densidades', Term);

  // Descontabilizar
  Term.SpanishTerm := 'Descontabilizar';
  Term.EnglishTerms := TArray<string>.Create(
    'ReversePosting',
    'UnpostTransaction',
    'UndoPosting',
    'ReverseEntry',
    'CancelPosting'
  );
  Term.Context := 'Action of reversing or canceling a posted journal entry. Creates a reversing entry to undo the original posting.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'AccountingProcess',
    'Reversals',
    'Posting'
  );
  FTerms.Add('Descontabilizar', Term);

  // Descripcion
  Term.SpanishTerm := 'Descripcion';
  Term.EnglishTerms := TArray<string>.Create(
    'Description',
    'WorkDescription',
    'Details',
    'Notes',
    'Comments'
  );
  Term.Context := 'A description or notes field (variant spelling without accent). Same as Descripción.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'DataProperty',
    'Documentation',
    'TextData'
  );
  FTerms.Add('Descripcion', Term);

  // Descripción
  Term.SpanishTerm := 'Descripción';
  Term.EnglishTerms := TArray<string>.Create(
    'Description',
    'Details',
    'LongDescription',
    'Notes',
    'WorkDescription',
    'Notes'
  );
  Term.Context := 'Description field. Extended descriptive text for entities.';
  Term.DomainTags := TArray<string>.Create(
    'DataFields',
    'Descriptors',
    'MasterData',
    'TextFields'
  );
  FTerms.Add('Descripción', Term);

  // Descuento
  Term.SpanishTerm := 'Descuento';
  Term.EnglishTerms := TArray<string>.Create(
    'Discount',
    'PriceReduction',
    'Markdown',
    'Rebate'
  );
  Term.Context := 'Discount applied to prices or invoices. Can be percentage or absolute amount.';
  Term.DomainTags := TArray<string>.Create(
    'Pricing',
    'Discounts',
    'SalesPromotion',
    'PriceManagement'
  );
  FTerms.Add('Descuento', Term);

  // Descuentos
  Term.SpanishTerm := 'Descuentos';
  Term.EnglishTerms := TArray<string>.Create(
    'Discounts',
    'PriceReductions',
    'Markdowns',
    'Rebates'
  );
  Term.Context := 'Plural form. Multiple discount schedules and promotions.';
  Term.DomainTags := TArray<string>.Create(
    'Pricing',
    'Discounts',
    'SalesPromotion',
    'PriceManagement'
  );
  FTerms.Add('Descuentos', Term);

  // Destruidas
  Term.SpanishTerm := 'Destruidas';
  Term.EnglishTerms := TArray<string>.Create(
    'Destroyed',
    'Scrapped',
    'Wasted',
    'Rejected',
    'Defective'
  );
  Term.Context := 'Units destroyed or scrapped due to defects';
  Term.DomainTags := TArray<string>.Create(
    'QualityControl',
    'ProductionTracking',
    'WasteManagement'
  );
  FTerms.Add('Destruidas', Term);

  // Dialogs
  Term.SpanishTerm := 'Dialogs';
  Term.EnglishTerms := TArray<string>.Create(
    'Dialogs',
    'DialogBoxes',
    'MessageDialogs',
    'UserDialogs'
  );
  Term.Context := 'Custom dialog framework wrapping VCL dialogs with application-specific styling and behavior.';
  Term.DomainTags := TArray<string>.Create(
    'UserInterface',
    'MessageDialogs',
    'UserInteraction'
  );
  FTerms.Add('Dialogs', Term);

  // Diario
  Term.SpanishTerm := 'Diario';
  Term.EnglishTerms := TArray<string>.Create(
    'GeneralJournal',
    'Journal',
    'DayBook',
    'JournalBook',
    'Diary'
  );
  Term.Context := 'General journal or daybook where all accounting transactions are chronologically recorded. Core accounting register in Spanish accounting system.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'JournalEntries',
    'AccountingBooks',
    'Accounting'
  );
  FTerms.Add('Diario', Term);

  // ============================================================
  // Category: MANUFACTURING EXECUTION
  // ============================================================

  // DiarioPr
  Term.SpanishTerm := 'DiarioPr';
  Term.EnglishTerms := TArray<string>.Create(
    'ProductionJournal',
    'DailyProduction',
    'ShopFloorLog',
    'ProductionLog'
  );
  Term.Context := 'Daily production tracking records for shop floor activities';
  Term.DomainTags := TArray<string>.Create(
    'ShopFloorControl',
    'TimeTracking',
    'ProductionTracking',
    'MES'
  );
  FTerms.Add('DiarioPr', Term);

  // Dirección
  Term.SpanishTerm := 'Dirección';
  Term.EnglishTerms := TArray<string>.Create(
    'Address',
    'StreetAddress',
    'Location',
    'PostalAddress'
  );
  Term.Context := 'Address field for customers, suppliers, warehouses. Physical location information.';
  Term.DomainTags := TArray<string>.Create(
    'DataFields',
    'GeographicData',
    'ContactInformation',
    'LocationData'
  );
  FTerms.Add('Dirección', Term);

  // Disponibilidad/Disponibilidades
  Term.SpanishTerm := 'Disponibilidad';
  Term.EnglishTerms := TArray<string>.Create(
    'Availability',
    'MediaAvailability',
    'BillboardAvailability',
    'InventoryAvailability',
    'Vacancy',
    'ResourceAvailability'
  );
  Term.Context := 'Billboard availability calendar tracking occupied and vacant periods for campaign planning and sales. Critical for inventory management and revenue optimization.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'InventoryManagement',
    'CapacityPlanning',
    'SalesOperations'
  );
  FTerms.Add('Disponibilidad', Term);

  // División
  Term.SpanishTerm := 'División';
  Term.EnglishTerms := TArray<string>.Create(
    'Division',
    'BusinessUnit',
    'Department',
    'Branch'
  );
  Term.Context := 'Business division or unit. Organizational subdivision for reporting and segmentation.';
  Term.DomainTags := TArray<string>.Create(
    'OrganizationalStructure',
    'BusinessUnits',
    'DepartmentManagement',
    'Segmentation'
  );
  FTerms.Add('División', Term);

  // Efecto
  Term.SpanishTerm := 'Efecto';
  Term.EnglishTerms := TArray<string>.Create(
    'BillOfExchange',
    'PromissoryNote',
    'PaymentEffect',
    'Receivable',
    'Payable',
    'Bill'
  );
  Term.Context := 'Bill of exchange or promissory note representing future payment/collection. Can be customer receivables or supplier payables.';
  Term.DomainTags := TArray<string>.Create(
    'AccountsReceivable',
    'AccountsPayable',
    'BankingOperations',
    'Collections'
  );
  FTerms.Add('Efecto', Term);

  // Efectos
  Term.SpanishTerm := 'Efectos';
  Term.EnglishTerms := TArray<string>.Create(
    'BillsOfExchange',
    'PromissoryNotes',
    'PaymentEffects',
    'Receivables',
    'Payables',
    'Bills'
  );
  Term.Context := 'Multiple bills of exchange or promissory notes. Managed in the bills module for collections and payments.';
  Term.DomainTags := TArray<string>.Create(
    'AccountsReceivable',
    'AccountsPayable',
    'BankingOperations',
    'Collections'
  );
  FTerms.Add('Efectos', Term);

  // Ejercicio
  Term.SpanishTerm := 'Ejercicio';
  Term.EnglishTerms := TArray<string>.Create(
    'FiscalYear',
    'AccountingYear',
    'FinancialYear',
    'Period',
    'Exercise'
  );
  Term.Context := 'Fiscal year or accounting period. All accounting data partitioned by Empresa and Ejercicio. Requires year selection for all operations.';
  Term.DomainTags := TArray<string>.Create(
    'FiscalYear',
    'PeriodManagement',
    'Accounting',
    'Configuration'
  );
  FTerms.Add('Ejercicio', Term);

  // Ejercicios
  Term.SpanishTerm := 'Ejercicios';
  Term.EnglishTerms := TArray<string>.Create(
    'FiscalYears',
    'AccountingYears',
    'FinancialYears',
    'Periods',
    'Exercises'
  );
  Term.Context := 'Multiple fiscal years or accounting periods. System maintains separate books for each fiscal year.';
  Term.DomainTags := TArray<string>.Create(
    'FiscalYear',
    'PeriodManagement',
    'Accounting'
  );
  FTerms.Add('Ejercicios', Term);

  // Embalaje
  Term.SpanishTerm := 'Embalaje';
  Term.EnglishTerms := TArray<string>.Create(
    'Packaging',
    'Packing',
    'Wrapping',
    'Container'
  );
  Term.Context := 'Product packaging and packing operations';
  Term.DomainTags := TArray<string>.Create(
    'Logistics',
    'Packaging',
    'Shipping',
    'WarehouseManagement'
  );
  FTerms.Add('Embalaje', Term);

  // Emitir
  Term.SpanishTerm := 'Emitir';
  Term.EnglishTerms := TArray<string>.Create(
    'Issue',
    'Emit',
    'Generate',
    'Release',
    'Create',
    'Produce'
  );
  Term.Context := 'Issue or emit document (invoice, order, etc.). Formally generate and release business document.';
  Term.DomainTags := TArray<string>.Create(
    'DocumentManagement',
    'Invoicing',
    'OrderManagement',
    'DocumentGeneration'
  );
  FTerms.Add('Emitir', Term);

  // Emplazamientos
  Term.SpanishTerm := 'Emplazamientos';
  Term.EnglishTerms := TArray<string>.Create(
    'Sites',
    'Locations',
    'BillboardSites',
    'MediaLocations',
    'AdvertisingSites',
    'Placements'
  );
  Term.Context := 'Physical sites/locations where billboards are installed including geographic coordinates, traffic data, visibility metrics, and ownership details.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'LocationManagement',
    'AssetManagement',
    'SiteManagement'
  );
  FTerms.Add('Emplazamientos', Term);

  // ============================================================
  // Category: FINANCIAL YEAR MANAGEMENT
  // ============================================================

  // Empresa
  Term.SpanishTerm := 'Empresa';
  Term.EnglishTerms := TArray<string>.Create(
    'Company',
    'Entity',
    'Organization',
    'Business',
    'LegalEntity',
    'Corporation'
  );
  Term.Context := 'Company or legal entity. All accounting data is partitioned by Empresa. Multi-company system requires company selection for all operations.';
  Term.DomainTags := TArray<string>.Create(
    'MultiCompany',
    'EntityManagement',
    'Configuration',
    'Accounting'
  );
  FTerms.Add('Empresa', Term);

  // Empresas
  Term.SpanishTerm := 'Empresas';
  Term.EnglishTerms := TArray<string>.Create(
    'Companies',
    'Entities',
    'Organizations',
    'Businesses',
    'LegalEntities',
    'Corporations'
  );
  Term.Context := 'Multiple companies or legal entities. System manages multiple separate companies with independent accounting books.';
  Term.DomainTags := TArray<string>.Create(
    'MultiCompany',
    'EntityManagement',
    'Configuration',
    'CompanyManagement'
  );
  FTerms.Add('Empresas', Term);

  // EnCurso
  Term.SpanishTerm := 'EnCurso';
  Term.EnglishTerms := TArray<string>.Create(
    'InProgress',
    'Active',
    'Running',
    'Ongoing'
  );
  Term.Context := 'Production phase currently being executed on shop floor';
  Term.DomainTags := TArray<string>.Create(
    'ProcessControl',
    'PhaseManagement',
    'ShopFloorControl'
  );
  FTerms.Add('EnCurso', Term);

  // Envia/Enviar
  Term.SpanishTerm := 'Envia';
  Term.EnglishTerms := TArray<string>.Create(
    'Send',
    'Transmit',
    'Submit',
    'Forward'
  );
  Term.Context := 'Send operation for emails, commands, or data. Used throughout communication subsystems.';
  Term.DomainTags := TArray<string>.Create(
    'Communication',
    'Email',
    'DataTransmission'
  );
  FTerms.Add('Envia', Term);

  // Enviar
  Term.SpanishTerm := 'Enviar';
  Term.EnglishTerms := TArray<string>.Create(
    'Send',
    'ToSend',
    'Transmit',
    'Submit'
  );
  Term.Context := 'Infinitive form of send operation. Used in method names and UI labels.';
  Term.DomainTags := TArray<string>.Create(
    'Communication',
    'Email',
    'DataTransmission'
  );
  FTerms.Add('Enviar', Term);

  // Envío
  Term.SpanishTerm := 'Envío';
  Term.EnglishTerms := TArray<string>.Create(
    'Shipment',
    'Shipping',
    'Dispatch',
    'Delivery'
  );
  Term.Context := 'Shipment or shipping operation. Outbound goods shipment to customer.';
  Term.DomainTags := TArray<string>.Create(
    'ShippingManagement',
    'Logistics',
    'Fulfillment',
    'Distribution'
  );
  FTerms.Add('Envío', Term);

  // Envíos
  Term.SpanishTerm := 'Envíos';
  Term.EnglishTerms := TArray<string>.Create(
    'Shipments',
    'Shippings',
    'Dispatches',
    'Deliveries'
  );
  Term.Context := 'Plural form. Multiple shipment operations.';
  Term.DomainTags := TArray<string>.Create(
    'ShippingManagement',
    'Logistics',
    'Fulfillment',
    'Distribution'
  );
  FTerms.Add('Envíos', Term);

  // Esperando
  Term.SpanishTerm := 'Esperando';
  Term.EnglishTerms := TArray<string>.Create(
    'Waiting',
    'Pending',
    'Blocked',
    'OnHold'
  );
  Term.Context := 'Production phase waiting for resources, materials, or previous phase completion';
  Term.DomainTags := TArray<string>.Create(
    'ProcessControl',
    'ResourcePlanning',
    'WorkflowStatus'
  );
  FTerms.Add('Esperando', Term);

  // Espere
  Term.SpanishTerm := 'Espere';
  Term.EnglishTerms := TArray<string>.Create(
    'Wait',
    'PleaseWait',
    'Loading',
    'Processing'
  );
  Term.Context := 'Wait indicator or message shown during operations. Short form of EsperePorFavor.';
  Term.DomainTags := TArray<string>.Create(
    'UserInterface',
    'ProgressIndicator',
    'UserExperience'
  );
  FTerms.Add('Espere', Term);

  // ============================================================
  // Category: USER INTERFACE COMPONENTS
  // ============================================================

  // EsperePorFavor
  Term.SpanishTerm := 'EsperePorFavor';
  Term.EnglishTerms := TArray<string>.Create(
    'PleaseWait',
    'WaitDialog',
    'ProgressDialog',
    'BusyIndicator'
  );
  Term.Context := 'Modal progress dialog displaying operation progress to users. Shows messages, progress bars, and cancellation options during long operations.';
  Term.DomainTags := TArray<string>.Create(
    'UserInterface',
    'ProgressIndicator',
    'UserExperience',
    'AsyncOperations'
  );
  FTerms.Add('EsperePorFavor', Term);

  // Estadisticas
  Term.SpanishTerm := 'Estadisticas';
  Term.EnglishTerms := TArray<string>.Create(
    'Statistics',
    'Metrics',
    'KPIs',
    'Analytics',
    'Data'
  );
  Term.Context := 'Production statistics and key performance indicators';
  Term.DomainTags := TArray<string>.Create(
    'Analytics',
    'Reporting',
    'PerformanceMetrics',
    'BusinessIntelligence'
  );
  FTerms.Add('Estadisticas', Term);

  // ============================================================
  // Category: STATUS & STATE
  // ============================================================

  // Estado
  Term.SpanishTerm := 'Estado';
  Term.EnglishTerms := TArray<string>.Create(
    'Status',
    'State',
    'ProjectStatus',
    'ProjectState',
    'Condition',
    'Phase'
  );
  Term.Context := 'Status or state of a project. Projects can be: New (N), In Progress (C), Blocked (B), Finished (T), or Archived (A). Used to track project lifecycle.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'StatusTracking',
    'ProjectLifecycle',
    'DataProperty'
  );
  FTerms.Add('Estado', Term);

  // Estructura
  Term.SpanishTerm := 'Estructura';
  Term.EnglishTerms := TArray<string>.Create(
    'Structure',
    'ProductStructure',
    'BOM',
    'Hierarchy'
  );
  Term.Context := 'Product structure and bill of materials hierarchy';
  Term.DomainTags := TArray<string>.Create(
    'ProductStructure',
    'BillOfMaterials',
    'Engineering'
  );
  FTerms.Add('Estructura', Term);

  // ============================================================
  // Category: EVENT SOURCING FIELDS
  // ============================================================

  // Evento
  Term.SpanishTerm := 'Evento';
  Term.EnglishTerms := TArray<string>.Create(
    'Event',
    'EventType',
    'Operation',
    'ChangeType',
    'TransactionType',
    'ActionType'
  );
  Term.Context := 'Type of event operation: = (set), + (increment), - (decrement). Defines how the event value should be applied to current state.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'CQRS',
    'OperationType',
    'StateManagement'
  );
  FTerms.Add('Evento', Term);

  // Eventos
  Term.SpanishTerm := 'Eventos';
  Term.EnglishTerms := TArray<string>.Create(
    'Events',
    'Transactions',
    'Changes',
    'Movements',
    'LogEntries',
    'AuditRecords'
  );
  Term.Context := 'Multiple product events. Records in the event sourcing system tracking inventory changes, price updates, and other product data modifications.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'CQRS',
    'AuditTrail',
    'InventoryTracking'
  );
  FTerms.Add('Eventos', Term);

  // EventosArticulos
  Term.SpanishTerm := 'EventosArticulos';
  Term.EnglishTerms := TArray<string>.Create(
    'ProductEvents',
    'ItemEvents',
    'ArticleEvents',
    'EventLogTable',
    'AuditTrailTable',
    'ProductEventsTable'
  );
  Term.Context := 'Full name of EvtArt table. System-managed ledger tracking all product data changes with complete audit trail. Events are created/updated/deleted by business logic when source documents change.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'CQRS',
    'DatabaseSchema',
    'AuditTrail'
  );
  FTerms.Add('EventosArticulos', Term);

  // EvtArt
  Term.SpanishTerm := 'EvtArt';
  Term.EnglishTerms := TArray<string>.Create(
    'ProductEvents',
    'ItemEvents',
    'EventLog',
    'AuditTrail',
    'EventSourcing',
    'TransactionLog'
  );
  Term.Context := 'System-managed ledger of product events (like General Ledger in accounting). Source of truth for all product data, automatically maintained by business logic.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'CQRS',
    'AuditTrail',
    'InventoryTracking'
  );
  FTerms.Add('EvtArt', Term);

  // ============================================================
  // Category: INVENTORY MANAGEMENT
  // ============================================================

  // Existencia
  Term.SpanishTerm := 'Existencia';
  Term.EnglishTerms := TArray<string>.Create(
    'Stock',
    'Inventory',
    'OnHand',
    'AvailableStock'
  );
  Term.Context := 'Stock or inventory level. Available quantity of product in warehouse.';
  Term.DomainTags := TArray<string>.Create(
    'Inventory',
    'StockControl',
    'WarehouseManagement',
    'InventoryManagement'
  );
  FTerms.Add('Existencia', Term);

  // Existencias
  Term.SpanishTerm := 'Existencias';
  Term.EnglishTerms := TArray<string>.Create(
    'Stocks',
    'Inventories',
    'OnHandQuantities',
    'AvailableStocks',
    'Stock',
    'Inventory'
  );
  Term.Context := 'Plural form. Stock levels across products and warehouses.';
  Term.DomainTags := TArray<string>.Create(
    'Inventory',
    'StockControl',
    'WarehouseManagement',
    'InventoryManagement'
  );
  FTerms.Add('Existencias', Term);

  // Expediente
  Term.SpanishTerm := 'Expediente';
  Term.EnglishTerms := TArray<string>.Create(
    'Dossier',
    'Case',
    'File',
    'ProjectFile'
  );
  Term.Context := 'Customer dossier or case file for project/opportunity management. Case management for complex sales or projects.';
  Term.DomainTags := TArray<string>.Create(
    'CaseManagement',
    'ProjectManagement',
    'CRM',
    'OpportunityManagement'
  );
  FTerms.Add('Expediente', Term);

  // Expedientes
  Term.SpanishTerm := 'Expedientes';
  Term.EnglishTerms := TArray<string>.Create(
    'Dossiers',
    'Cases',
    'Files',
    'ProjectFiles'
  );
  Term.Context := 'Plural form. Multiple case files and project dossiers.';
  Term.DomainTags := TArray<string>.Create(
    'CaseManagement',
    'ProjectManagement',
    'CRM',
    'OpportunityManagement'
  );
  FTerms.Add('Expedientes', Term);

  // Exportar
  Term.SpanishTerm := 'Exportar';
  Term.EnglishTerms := TArray<string>.Create(
    'Export',
    'ExportData',
    'DataExport',
    'OutputData',
    'GenerateExport',
    'OutputData'
  );
  Term.Context := 'Export data to external system or file. Data integration and external system synchronization.';
  Term.DomainTags := TArray<string>.Create(
    'DataExchange',
    'Integration',
    'EDI',
    'DataExport'
  );
  FTerms.Add('Exportar', Term);

  // Extracto
  Term.SpanishTerm := 'Extracto';
  Term.EnglishTerms := TArray<string>.Create(
    'BankStatement',
    'Statement',
    'AccountStatement',
    'BankExtract',
    'BankRecord'
  );
  Term.Context := 'Bank account statement for reconciliation. Imported from bank and matched against accounting entries (punteo).';
  Term.DomainTags := TArray<string>.Create(
    'BankingOperations',
    'Reconciliation',
    'CashManagement'
  );
  FTerms.Add('Extracto', Term);

  // Extractos
  Term.SpanishTerm := 'Extractos';
  Term.EnglishTerms := TArray<string>.Create(
    'BankStatements',
    'Statements',
    'AccountStatements',
    'BankExtracts',
    'BankRecords',
    'Extracts'
  );
  Term.Context := 'Multiple bank account statements. Used for reconciliation across multiple bank accounts or periods.';
  Term.DomainTags := TArray<string>.Create(
    'BankingOperations',
    'Reconciliation',
    'CashManagement',
    'Reporting'
  );
  FTerms.Add('Extractos', Term);

  // FIDE
  Term.SpanishTerm := 'FIDE';
  Term.EnglishTerms := TArray<string>.Create(
    'FIDE',
    'LoyaltyCard',
    'CustomerLoyalty',
    'LoyaltyProgram'
  );
  Term.Context := 'FIDE - Spanish loyalty card system integration. Customer loyalty program with card-based rewards.';
  Term.DomainTags := TArray<string>.Create(
    'LoyaltyPrograms',
    'CustomerRetention',
    'Marketing',
    'Rewards'
  );
  FTerms.Add('FIDE', Term);

  // FONDO
  Term.SpanishTerm := 'FONDO';
  Term.EnglishTerms := TArray<string>.Create(
    'Depth',
    'ProductDepth',
    'ItemDepth',
    'Dimension',
    'DepthDimension',
    'DepthSpec'
  );
  Term.Context := 'Product depth dimension for storage and shipping planning. Non-incrementable (=) class.';
  Term.DomainTags := TArray<string>.Create(
    'ProductSpecifications',
    'WarehouseManagement',
    'ShippingManagement',
    'SpacePlanning'
  );
  FTerms.Add('FONDO', Term);

  // Factura
  Term.SpanishTerm := 'Factura';
  Term.EnglishTerms := TArray<string>.Create(
    'Invoice',
    'Bill',
    'SalesInvoice',
    'Document',
    'InvoiceDocument',
    'ProjectInvoice'
  );
  Term.Context := 'Invoice document for sales or purchases. Tracked in VAT books and reported to Spanish tax authority via SII system.';
  Term.DomainTags := TArray<string>.Create(
    'Invoicing',
    'TaxCompliance',
    'VAT',
    'SalesProcessing'
  );
  FTerms.Add('Factura', Term);

  // Facturación
  Term.SpanishTerm := 'Facturación';
  Term.EnglishTerms := TArray<string>.Create(
    'Invoicing',
    'Billing',
    'InvoiceGeneration',
    'BillingProcess',
    'AccountsReceivable'
  );
  Term.Context := 'Invoicing process for generating customer bills from orders, services, or time records. Critical financial workflow in all business modules.';
  Term.DomainTags := TArray<string>.Create(
    'Accounting',
    'Billing',
    'BusinessProcesses',
    'SalesManagement'
  );
  FTerms.Add('Facturación', Term);

  // Facturae
  Term.SpanishTerm := 'Facturae';
  Term.EnglishTerms := TArray<string>.Create(
    'Facturae',
    'ElectronicInvoicing',
    'eInvoicing',
    'StructuredInvoice'
  );
  Term.Context := 'Facturae - Spanish electronic invoicing XML format. B2B and B2G (government) e-invoicing standard (versions 3.2.1, 3.2.2).';
  Term.DomainTags := TArray<string>.Create(
    'ElectronicInvoicing',
    'B2B',
    'B2G',
    'XMLInvoicing'
  );
  FTerms.Add('Facturae', Term);

  // Facturas
  Term.SpanishTerm := 'Facturas';
  Term.EnglishTerms := TArray<string>.Create(
    'Invoices',
    'Bills',
    'SalesInvoices',
    'InvoiceDocuments',
    'BillingDocuments',
    'InvoiceDocuments'
  );
  Term.Context := 'Multiple invoice documents. Used in batch processing, VAT reporting, and SII submissions.';
  Term.DomainTags := TArray<string>.Create(
    'Invoicing',
    'TaxCompliance',
    'VAT',
    'SalesProcessing'
  );
  FTerms.Add('Facturas', Term);

  // Familia
  Term.SpanishTerm := 'Familia';
  Term.EnglishTerms := TArray<string>.Create(
    'Family',
    'ProductFamily',
    'Category',
    'ProductCategory',
    'Group'
  );
  Term.Context := 'Product family or category for grouping related articles. Used for reporting and pricing rules.';
  Term.DomainTags := TArray<string>.Create(
    'ProductManagement',
    'CatalogManagement',
    'ProductHierarchy',
    'Classification'
  );
  FTerms.Add('Familia', Term);

  // Familias
  Term.SpanishTerm := 'Familias';
  Term.EnglishTerms := TArray<string>.Create(
    'Families',
    'ProductFamilies',
    'Categories',
    'ProductCategories',
    'Groups',
    'Classifications'
  );
  Term.Context := 'Plural form. Product classification hierarchy.';
  Term.DomainTags := TArray<string>.Create(
    'ProductManagement',
    'CatalogManagement',
    'ProductHierarchy',
    'Classification'
  );
  FTerms.Add('Familias', Term);

  // ============================================================
  // Category: DOCUMENT PROPERTIES
  // ============================================================

  // Fecha
  Term.SpanishTerm := 'Fecha';
  Term.EnglishTerms := TArray<string>.Create(
    'Date',
    'WorkDate',
    'EntryDate',
    'TransactionDate',
    'RecordDate',
    'DateTime'
  );
  Term.Context := 'A date field on documents and work reports. Used for work date, start date, end date, and other temporal tracking in projects.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'TimeTracking',
    'DataProperty',
    'Temporal'
  );
  FTerms.Add('Fecha', Term);

  // Ficha
  Term.SpanishTerm := 'Ficha';
  Term.EnglishTerms := TArray<string>.Create(
    'Form',
    'DetailForm',
    'RecordCard',
    'DataForm',
    'EntityForm'
  );
  Term.Context := 'Detail form displaying and editing a single database record. Contrasts with list views showing multiple records.';
  Term.DomainTags := TArray<string>.Create(
    'UserInterface',
    'FormDesign',
    'DataEntry',
    'RecordEditing'
  );
  FTerms.Add('Ficha', Term);

  // Fichajes
  Term.SpanishTerm := 'Fichajes';
  Term.EnglishTerms := TArray<string>.Create(
    'TimeTracking',
    'ClockIn',
    'ClockInOut',
    'ProductionTracking',
    'LaborTracking',
    'TimeRecording'
  );
  Term.Context := 'Production time tracking for shop floor operations recording worker time on production orders, machines, and operations. Essential for labor cost tracking and MES.';
  Term.DomainTags := TArray<string>.Create(
    'Manufacturing',
    'TimeTracking',
    'LaborManagement',
    'MES'
  );
  FTerms.Add('Fichajes', Term);

  // Fichar
  Term.SpanishTerm := 'Fichar';
  Term.EnglishTerms := TArray<string>.Create(
    'ClockIn',
    'RecordTime',
    'TrackTime',
    'LogTime',
    'RegisterTime'
  );
  Term.Context := 'Action of recording production time when workers start/stop work on orders or machines. Critical shop floor operation for real-time production tracking.';
  Term.DomainTags := TArray<string>.Create(
    'Manufacturing',
    'TimeTracking',
    'ShopFloorControl',
    'LaborManagement'
  );
  FTerms.Add('Fichar', Term);

  // ============================================================
  // Category: FILE & EMAIL OPERATIONS
  // ============================================================

  // Fichero
  Term.SpanishTerm := 'Fichero';
  Term.EnglishTerms := TArray<string>.Create(
    'File',
    'Document',
    'DataFile',
    'Attachment'
  );
  Term.Context := 'File or document handling. Used for file operations, attachments, and file-based data storage.';
  Term.DomainTags := TArray<string>.Create(
    'FileManagement',
    'DocumentHandling',
    'Storage'
  );
  FTerms.Add('Fichero', Term);

  // Fijaciones
  Term.SpanishTerm := 'Fijaciones';
  Term.EnglishTerms := TArray<string>.Create(
    'Installations',
    'Postings',
    'BillboardInstallations',
    'CampaignExecutions',
    'MediaPostings'
  );
  Term.Context := 'Physical installation operations for advertising campaigns including scheduling, execution tracking, and quality control. Operational execution entity.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'OperationsManagement',
    'FieldOperations',
    'CampaignExecution'
  );
  FTerms.Add('Fijaciones', Term);

  // Filtro
  Term.SpanishTerm := 'Filtro';
  Term.EnglishTerms := TArray<string>.Create(
    'Filter',
    'DataFilter',
    'RecordFilter',
    'QueryFilter'
  );
  Term.Context := 'Visual database filter designer allowing users to build complex WHERE clauses through UI. Saves and loads filter configurations.';
  Term.DomainTags := TArray<string>.Create(
    'DatabaseAccess',
    'QueryBuilder',
    'UserInterface',
    'DataFiltering'
  );
  FTerms.Add('Filtro', Term);

  // Filtros
  Term.SpanishTerm := 'Filtros';
  Term.EnglishTerms := TArray<string>.Create(
    'Filters',
    'DataFilters',
    'RecordFilters'
  );
  Term.Context := 'Multiple database filters or filter management system.';
  Term.DomainTags := TArray<string>.Create(
    'DatabaseAccess',
    'QueryBuilder',
    'DataFiltering'
  );
  FTerms.Add('Filtros', Term);

  // Aeropuerto (singular)
  Term.SpanishTerm := 'Aeropuerto';
  Term.EnglishTerms := TArray<string>.Create(
    'Airport',
    'Aerodrome',
    'Airfield'
  );
  Term.Context := 'Individual airport facility with operational codes and infrastructure data.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'MasterData',
    'Infrastructure'
  );
  FTerms.Add('Aeropuerto', Term);

  // ============================================================
  // Category: AVIATION
  Term.SpanishTerm := 'Flota';
  Term.EnglishTerms := TArray<string>.Create(
    'Fleet',
    'AircraftFleet',
    'FleetManagement',
    'AircraftInventory'
  );
  Term.Context := 'Aircraft fleet management including registrations, types, weights, capacities, and operational parameters. Tracks all aircraft serviced by the FBO.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'FleetManagement',
    'AssetManagement',
    'MasterData'
  );
  FTerms.Add('Flota', Term);

  // FormaPago
  Term.SpanishTerm := 'FormaPago';
  Term.EnglishTerms := TArray<string>.Create(
    'PaymentMethod',
    'PaymentTerm',
    'PaymentType',
    'PaymentMode'
  );
  Term.Context := 'Payment method or terms (cash, bank transfer, direct debit, etc.). Defines how and when payment is made.';
  Term.DomainTags := TArray<string>.Create(
    'PaymentProcessing',
    'PaymentTerms',
    'AccountsReceivable',
    'AccountsPayable'
  );
  FTerms.Add('FormaPago', Term);

  // FormasPago
  Term.SpanishTerm := 'FormasPago';
  Term.EnglishTerms := TArray<string>.Create(
    'PaymentMethods',
    'PaymentTerms',
    'PaymentTypes',
    'PaymentModes'
  );
  Term.Context := 'Plural form. Multiple payment method configurations.';
  Term.DomainTags := TArray<string>.Create(
    'PaymentProcessing',
    'PaymentTerms',
    'AccountsReceivable',
    'AccountsPayable'
  );
  FTerms.Add('FormasPago', Term);

  // Formulario
  Term.SpanishTerm := 'Formulario';
  Term.EnglishTerms := TArray<string>.Create(
    'Form',
    'Window',
    'Dialog',
    'Screen'
  );
  Term.Context := 'Application window or form. Generic term for any screen or dialog in the application.';
  Term.DomainTags := TArray<string>.Create(
    'UserInterface',
    'FormManagement',
    'WindowManagement'
  );
  FTerms.Add('Formulario', Term);

  // Gasto
  Term.SpanishTerm := 'Gasto';
  Term.EnglishTerms := TArray<string>.Create(
    'Expense',
    'Cost',
    'Charge',
    'Expenditure',
    'Expenditure',
    'OutOfPocketCost'
  );
  Term.Context := 'Individual overhead cost item';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'Finance',
    'Overhead',
    'ProjectManagement'
  );
  FTerms.Add('Gasto', Term);

  // ============================================================
  // Category: COST MANAGEMENT
  // ============================================================

  // Gastos
  Term.SpanishTerm := 'Gastos';
  Term.EnglishTerms := TArray<string>.Create(
    'Expenses',
    'Overhead',
    'Costs',
    'Charges',
    'Expenditures',
    'Expenditures'
  );
  Term.Context := 'Production overhead and indirect costs';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'Manufacturing',
    'Finance',
    'Overhead'
  );
  FTerms.Add('Gastos', Term);

  // Generar
  Term.SpanishTerm := 'Generar';
  Term.EnglishTerms := TArray<string>.Create(
    'Generate',
    'Create',
    'Produce',
    'Build',
    'Build',
    'Make'
  );
  Term.Context := 'Generate documents or data. Automated creation of invoices, orders, or other documents.';
  Term.DomainTags := TArray<string>.Create(
    'DocumentGeneration',
    'Automation',
    'DataGeneration',
    'ProcessAutomation'
  );
  FTerms.Add('Generar', Term);

  // Grid
  Term.SpanishTerm := 'Grid';
  Term.EnglishTerms := TArray<string>.Create(
    'Grid',
    'DataGrid',
    'TableGrid',
    'DBGrid'
  );
  Term.Context := 'Data-aware grid control for displaying and editing tabular data with custom column management.';
  Term.DomainTags := TArray<string>.Create(
    'UserInterface',
    'DataComponents',
    'GridControl',
    'DataDisplay'
  );
  FTerms.Add('Grid', Term);

  // GrupoComercial
  Term.SpanishTerm := 'GrupoComercial';
  Term.EnglishTerms := TArray<string>.Create(
    'CommercialGroup',
    'CustomerGroup',
    'BusinessGroup',
    'AccountGroup'
  );
  Term.Context := 'Commercial or customer group for segmentation and pricing. Logical grouping of customers for business rules.';
  Term.DomainTags := TArray<string>.Create(
    'CustomerSegmentation',
    'CustomerManagement',
    'BusinessIntelligence',
    'MarketSegmentation'
  );
  FTerms.Add('GrupoComercial', Term);

  // GruposComercial
  Term.SpanishTerm := 'GruposComercial';
  Term.EnglishTerms := TArray<string>.Create(
    'CommercialGroups',
    'CustomerGroups',
    'BusinessGroups',
    'AccountGroups'
  );
  Term.Context := 'Plural form. Customer segmentation structure.';
  Term.DomainTags := TArray<string>.Create(
    'CustomerSegmentation',
    'CustomerManagement',
    'BusinessIntelligence',
    'MarketSegmentation'
  );
  FTerms.Add('GruposComercial', Term);

  // Guardar
  Term.SpanishTerm := 'Guardar';
  Term.EnglishTerms := TArray<string>.Create(
    'Save',
    'Store',
    'Persist',
    'Write'
  );
  Term.Context := 'Save operation for data or configurations. Writes changes from memory to persistent storage.';
  Term.DomainTags := TArray<string>.Create(
    'DataManagement',
    'Persistence',
    'FileManagement'
  );
  FTerms.Add('Guardar', Term);

  // Hacienda
  Term.SpanishTerm := 'Hacienda';
  Term.EnglishTerms := TArray<string>.Create(
    'TaxAuthority',
    'SpanishIRS',
    'AEAT',
    'TaxOffice',
    'RevenueService'
  );
  Term.Context := 'Spanish tax authority (Agencia Estatal de Administración Tributaria - AEAT). Recipient of tax declarations and SII submissions.';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'SpanishTax',
    'Government',
    'Regulatory'
  );
  FTerms.Add('Hacienda', Term);

  // ============================================================
  // Category: AVIATION
  Term.SpanishTerm := 'Handling';
  Term.EnglishTerms := TArray<string>.Create(
    'GroundHandling',
    'HandlingServices',
    'GroundServices',
    'RampServices',
    'AircraftHandling'
  );
  Term.Context := 'Ground handling service providers offering ramp services, baggage handling, fueling, and aircraft servicing. Essential FBO partners.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'GroundServices',
    'ServiceProviders',
    'FBOoperations'
  );
  FTerms.Add('Handling', Term);

  // Hospedaje/Hospedajes
  Term.SpanishTerm := 'Hospedaje';
  Term.EnglishTerms := TArray<string>.Create(
    'Accommodation',
    'CrewAccommodation',
    'HotelBooking',
    'Lodging',
    'CrewLodging'
  );
  Term.Context := 'Crew hotel accommodation bookings including dates, hotel, rooms, and billing. Critical for crew service coordination.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'CrewServices',
    'Accommodation',
    'ServiceCoordination'
  );
  FTerms.Add('Hospedaje', Term);

  // Hoteles
  Term.SpanishTerm := 'Hoteles';
  Term.EnglishTerms := TArray<string>.Create(
    'Hotels',
    'Accommodations',
    'LodgingFacilities',
    'CrewHotels'
  );
  Term.Context := 'Hotel master data including contact information, rates, and contracts for crew accommodation services.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'CrewServices',
    'MasterData',
    'SupplierManagement'
  );
  FTerms.Add('Hoteles', Term);

  // IRPF
  Term.SpanishTerm := 'IRPF';
  Term.EnglishTerms := TArray<string>.Create(
    'IncomeTaxWithholding',
    'WithholdingTax',
    'PersonalIncomeTax',
    'TaxRetention',
    'IncomeTaxRetention',
    'TaxWithholding'
  );
  Term.Context := 'Impuesto sobre la Renta de las Personas Físicas - Spanish personal income tax withholding. Applied to professional services and salaries.';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'SpanishTax',
    'Withholding',
    'Invoicing'
  );
  FTerms.Add('IRPF', Term);

  // ============================================================
  // Category: VAT & TAX COMPLIANCE
  // ============================================================

  // IVA
  Term.SpanishTerm := 'IVA';
  Term.EnglishTerms := TArray<string>.Create(
    'VAT',
    'ValueAddedTax',
    'SalesTax',
    'TaxOnValue',
    'ConsumptionTax',
    'TaxAmount'
  );
  Term.Context := 'Impuesto sobre el Valor Añadido - Spanish Value Added Tax. Critical tax type managed with separate VAT books and electronic reporting to tax authorities.';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'VAT',
    'SpanishTax',
    'Invoicing'
  );
  FTerms.Add('IVA', Term);

  // Importar
  Term.SpanishTerm := 'Importar';
  Term.EnglishTerms := TArray<string>.Create(
    'Import',
    'ImportData',
    'DataImport',
    'LoadData',
    'LoadData',
    'IntegrateData'
  );
  Term.Context := 'Import data from external system or file. Bulk data loading and external system integration.';
  Term.DomainTags := TArray<string>.Create(
    'DataExchange',
    'Integration',
    'EDI',
    'DataImport'
  );
  FTerms.Add('Importar', Term);

  // Importe
  Term.SpanishTerm := 'Importe';
  Term.EnglishTerms := TArray<string>.Create(
    'Amount',
    'TotalAmount',
    'LineTotal',
    'ExtendedAmount',
    'Sum',
    'Value'
  );
  Term.Context := 'A total amount or sum. Calculated by multiplying quantity by price or cost. Used throughout projects for line totals, subtotals, and grand totals.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'JobCosting',
    'Financial',
    'Calculation'
  );
  FTerms.Add('Importe', Term);

  // Importes
  Term.SpanishTerm := 'Importes';
  Term.EnglishTerms := TArray<string>.Create(
    'Amounts',
    'TotalAmounts',
    'LineTotals',
    'ExtendedAmounts',
    'Sums'
  );
  Term.Context := 'Multiple amounts or sums. Financial totals across different lines and categories in projects.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'JobCosting',
    'Financial',
    'Calculation'
  );
  FTerms.Add('Importes', Term);

  // Imprime
  Term.SpanishTerm := 'Imprime';
  Term.EnglishTerms := TArray<string>.Create(
    'Print',
    'Prints',
    'PrintOut',
    'Output',
    'Generate'
  );
  Term.Context := 'Print verb or printing module. Used in file names like "CuentasImprime" for printing reports.';
  Term.DomainTags := TArray<string>.Create(
    'Reporting',
    'Printing',
    'Output'
  );
  FTerms.Add('Imprime', Term);

  // ============================================================
  // Category: OPERATIONS & ACTIONS
  // ============================================================

  // Imprimir
  Term.SpanishTerm := 'Imprimir';
  Term.EnglishTerms := TArray<string>.Create(
    'Print',
    'PrintDocument',
    'GenerateReport',
    'OutputDocument'
  );
  Term.Context := 'Print document or report. Generate physical or PDF output from data.';
  Term.DomainTags := TArray<string>.Create(
    'DocumentManagement',
    'Reporting',
    'OutputGeneration',
    'PrintManagement'
  );
  FTerms.Add('Imprimir', Term);

  // Informe
  Term.SpanishTerm := 'Informe';
  Term.EnglishTerms := TArray<string>.Create(
    'Report',
    'Analysis',
    'Statement',
    'AnalysisReport',
    'InformationReport',
    'Document'
  );
  Term.Context := 'Analytical report or information report. More sophisticated than simple listings, includes analysis and summaries.';
  Term.DomainTags := TArray<string>.Create(
    'Reporting',
    'Analysis',
    'BusinessIntelligence',
    'Analytics'
  );
  FTerms.Add('Informe', Term);

  // ============================================================
  // Category: REPORTING
  // ============================================================

  // Informes
  Term.SpanishTerm := 'Informes';
  Term.EnglishTerms := TArray<string>.Create(
    'Reports',
    'Analytics',
    'Documents',
    'Statements'
  );
  Term.Context := 'Production reports and analytics for management decision-making';
  Term.DomainTags := TArray<string>.Create(
    'Reporting',
    'Analytics',
    'BusinessIntelligence',
    'Management'
  );
  FTerms.Add('Informes', Term);

  // Inventario
  Term.SpanishTerm := 'Inventario';
  Term.EnglishTerms := TArray<string>.Create(
    'Inventory',
    'StockCount',
    'PhysicalInventory',
    'StockTaking',
    'InventoryCount',
    'StockLevel'
  );
  Term.Context := 'Inventory calculation and physical count operations. Calculates current stock levels from event log or performs physical stock verification.';
  Term.DomainTags := TArray<string>.Create(
    'InventoryControl',
    'StockTaking',
    'WarehouseManagement',
    'StockVerification'
  );
  FTerms.Add('Inventario', Term);

  // Invertido
  Term.SpanishTerm := 'Invertido';
  Term.EnglishTerms := TArray<string>.Create(
    'Invested',
    'Spent',
    'Used',
    'Consumed',
    'Expended'
  );
  Term.Context := 'Actual time or cost invested/spent in production';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'TimeTracking',
    'Actuals',
    'Metrics'
  );
  FTerms.Add('Invertido', Term);

  // Lanzado
  Term.SpanishTerm := 'Lanzado';
  Term.EnglishTerms := TArray<string>.Create(
    'Launched',
    'Started',
    'Running'
  );
  Term.Context := 'Launched state indicator. Shows that a process or server has been started.';
  Term.DomainTags := TArray<string>.Create(
    'ProcessManagement',
    'StateManagement'
  );
  FTerms.Add('Lanzado', Term);

  // Lanzar
  Term.SpanishTerm := 'Lanzar';
  Term.EnglishTerms := TArray<string>.Create(
    'Launch',
    'Start',
    'Execute',
    'Run'
  );
  Term.Context := 'Launch operation for processes, servers, or operations. Starts execution of components.';
  Term.DomainTags := TArray<string>.Create(
    'ProcessManagement',
    'Execution',
    'ApplicationLifecycle'
  );
  FTerms.Add('Lanzar', Term);

  // Libro
  Term.SpanishTerm := 'Libro';
  Term.EnglishTerms := TArray<string>.Create(
    'VATBook',
    'TaxBook',
    'InvoiceRegister',
    'Book',
    'LedgerBook'
  );
  Term.Context := 'VAT book register (issued or received invoices). Required by Spanish law to track all VAT-bearing transactions.';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'VAT',
    'SpanishTax',
    'AccountingBooks'
  );
  FTerms.Add('Libro', Term);

  // Libros
  Term.SpanishTerm := 'Libros';
  Term.EnglishTerms := TArray<string>.Create(
    'VATBooks',
    'TaxBooks',
    'InvoiceRegisters',
    'Books',
    'AccountingBooks'
  );
  Term.Context := 'Multiple VAT book registers. Companies maintain separate books for issued and received invoices.';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'VAT',
    'SpanishTax',
    'AccountingBooks'
  );
  FTerms.Add('Libros', Term);

  // Licencia
  Term.SpanishTerm := 'Licencia';
  Term.EnglishTerms := TArray<string>.Create(
    'License',
    'SoftwareLicense',
    'LicenseKey',
    'Authorization'
  );
  Term.Context := 'Software license controlling application features, user count, and expiration. Managed by protection server.';
  Term.DomainTags := TArray<string>.Create(
    'Licensing',
    'CopyProtection',
    'Authorization',
    'SoftwareActivation'
  );
  FTerms.Add('Licencia', Term);

  // Liquidaciones
  Term.SpanishTerm := 'Liquidaciones';
  Term.EnglishTerms := TArray<string>.Create(
    'Settlements',
    'Liquidations',
    'AgentSettlements',
    'CommissionSettlements'
  );
  Term.Context := 'Plural form. Multiple settlement processes and documents.';
  Term.DomainTags := TArray<string>.Create(
    'CommissionManagement',
    'AgentCompensation',
    'PaymentProcessing',
    'FinancialSettlement'
  );
  FTerms.Add('Liquidaciones', Term);

  // Liquidación
  Term.SpanishTerm := 'Liquidación';
  Term.EnglishTerms := TArray<string>.Create(
    'Settlement',
    'Liquidation',
    'AgentSettlement',
    'CommissionSettlement'
  );
  Term.Context := 'Agent commission settlement or payment. Process of calculating and paying agent commissions.';
  Term.DomainTags := TArray<string>.Create(
    'CommissionManagement',
    'AgentCompensation',
    'PaymentProcessing',
    'FinancialSettlement'
  );
  FTerms.Add('Liquidación', Term);

  // Liquidar
  Term.SpanishTerm := 'Liquidar';
  Term.EnglishTerms := TArray<string>.Create(
    'Settle',
    'Liquidate',
    'Pay',
    'ClearPayment',
    'DischargeDebt'
  );
  Term.Context := 'Action of settling or liquidating bills, payments, or obligations. Marks items as paid/collected and optionally posts to accounting.';
  Term.DomainTags := TArray<string>.Create(
    'PaymentProcessing',
    'AccountsReceivable',
    'AccountsPayable',
    'Collections'
  );
  FTerms.Add('Liquidar', Term);

  // Listado
  Term.SpanishTerm := 'Listado';
  Term.EnglishTerms := TArray<string>.Create(
    'Report',
    'Listing',
    'PrintOut',
    'ReportOutput',
    'List'
  );
  Term.Context := 'Printed report or listing. Generic term for any report output from the system.';
  Term.DomainTags := TArray<string>.Create(
    'Reporting',
    'Printing',
    'Output'
  );
  FTerms.Add('Listado', Term);

  // Listar
  Term.SpanishTerm := 'Listar';
  Term.EnglishTerms := TArray<string>.Create(
    'List',
    'Report',
    'GenerateReport',
    'PrintList'
  );
  Term.Context := 'Generate list or report. Create tabular output or detailed report from data.';
  Term.DomainTags := TArray<string>.Create(
    'Reporting',
    'BusinessIntelligence',
    'DataAnalysis',
    'ReportGeneration'
  );
  FTerms.Add('Listar', Term);

  // Lookup
  Term.SpanishTerm := 'Lookup';
  Term.EnglishTerms := TArray<string>.Create(
    'Lookup',
    'LookupField',
    'ForeignKey',
    'RelatedData'
  );
  Term.Context := 'Lookup control for selecting related records from another table. Displays foreign key relationships.';
  Term.DomainTags := TArray<string>.Create(
    'UserInterface',
    'DataComponents',
    'RelationalData',
    'ForeignKeys'
  );
  FTerms.Add('Lookup', Term);

  // Lote
  Term.SpanishTerm := 'Lote';
  Term.EnglishTerms := TArray<string>.Create(
    'Batch',
    'Lot',
    'LotNumber',
    'BatchNumber',
    'BatchNumber',
    'ProductionLot'
  );
  Term.Context := 'Batch or lot tracking for inventory. Used for expirable products and traceability requirements.';
  Term.DomainTags := TArray<string>.Create(
    'LotTracking',
    'Traceability',
    'Inventory',
    'QualityControl'
  );
  FTerms.Add('Lote', Term);

  // Lotes
  Term.SpanishTerm := 'Lotes';
  Term.EnglishTerms := TArray<string>.Create(
    'Batches',
    'Lots',
    'LotNumbers',
    'BatchNumbers'
  );
  Term.Context := 'Plural form. Multiple lot numbers for tracking.';
  Term.DomainTags := TArray<string>.Create(
    'LotTracking',
    'Traceability',
    'Inventory',
    'QualityControl'
  );
  FTerms.Add('Lotes', Term);

  // Línea
  Term.SpanishTerm := 'Línea';
  Term.EnglishTerms := TArray<string>.Create(
    'Line',
    'DocumentLine',
    'OrderLine',
    'InvoiceLine',
    'ProductionLine',
    'ManufacturingLine'
  );
  Term.Context := 'Document line item containing product details, quantities, prices. Detail lines for invoices, orders, quotes.';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'Invoicing',
    'DocumentManagement',
    'LineItems'
  );
  FTerms.Add('Línea', Term);

  // Líneas
  Term.SpanishTerm := 'Líneas';
  Term.EnglishTerms := TArray<string>.Create(
    'Lines',
    'DocumentLines',
    'OrderLines',
    'InvoiceLines'
  );
  Term.Context := 'Plural form. Multiple line items in documents.';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'Invoicing',
    'DocumentManagement',
    'LineItems'
  );
  FTerms.Add('Líneas', Term);

  // Margen
  Term.SpanishTerm := 'Margen';
  Term.EnglishTerms := TArray<string>.Create(
    'Margin',
    'ProfitMargin',
    'GrossMargin',
    'Profit',
    'Markup',
    'ProfitPercentage'
  );
  Term.Context := 'Profit margin on a project or line. Calculated as the difference between price and cost. Key metric for project profitability analysis.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'JobCosting',
    'Profitability',
    'Financial'
  );
  FTerms.Add('Margen', Term);

  // Material
  Term.SpanishTerm := 'Material';
  Term.EnglishTerms := TArray<string>.Create(
    'Material',
    'RawMaterial',
    'Component',
    'Supply',
    'MaterialCost',
    'MaterialItem'
  );
  Term.Context := 'Individual material or component';
  Term.DomainTags := TArray<string>.Create(
    'MaterialPlanning',
    'Inventory',
    'BillOfMaterials',
    'ProjectManagement'
  );
  FTerms.Add('Material', Term);

  // ============================================================
  // Category: MATERIALS & INVENTORY
  // ============================================================

  // Materiales
  Term.SpanishTerm := 'Materiales';
  Term.EnglishTerms := TArray<string>.Create(
    'Materials',
    'RawMaterials',
    'Components',
    'Supplies',
    'MaterialCosts',
    'MaterialItems'
  );
  Term.Context := 'Raw materials and components consumed in production';
  Term.DomainTags := TArray<string>.Create(
    'MaterialPlanning',
    'Inventory',
    'BillOfMaterials',
    'SupplyChain'
  );
  FTerms.Add('Materiales', Term);

  // Matrícula
  Term.SpanishTerm := 'Matrícula';
  Term.EnglishTerms := TArray<string>.Create(
    'Registration',
    'TailNumber',
    'AircraftRegistration',
    'RegistrationMark',
    'CallSign'
  );
  Term.Context := 'Aircraft registration identifier (tail number) used for identification, pricing, and regulatory compliance. Unique identifier for each aircraft.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'FleetManagement',
    'Identification',
    'RegulatoryCompliance'
  );
  FTerms.Add('Matrícula', Term);

  // Mayor
  Term.SpanishTerm := 'Mayor';
  Term.EnglishTerms := TArray<string>.Create(
    'GeneralLedger',
    'Ledger',
    'MainLedger',
    'BookOfAccounts',
    'AccountingLedger'
  );
  Term.Context := 'General ledger showing account-by-account summary of all transactions. Organized by chart of accounts.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'AccountingBooks',
    'Accounting',
    'FinancialReporting'
  );
  FTerms.Add('Mayor', Term);

  // ============================================================
  // Category: AVIATION
  Term.SpanishTerm := 'Mensajería';
  Term.EnglishTerms := TArray<string>.Create(
    'Messaging',
    'AviationMessaging',
    'FlightMessaging',
    'SITAmessaging',
    'OperationalMessaging'
  );
  Term.Context := 'Aviation messaging system for slot coordination, flight notifications, and operational communication following SITA standards (SCR, MVT, RQP, CFM messages).';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'Communication',
    'SlotCoordination',
    'RegulatoryCompliance'
  );
  FTerms.Add('Mensajería', Term);

  // Mensajes
  Term.SpanishTerm := 'Mensajes';
  Term.EnglishTerms := TArray<string>.Create(
    'Messages',
    'AviationMessages',
    'FlightMessages',
    'OperationalMessages',
    'SITAmessages'
  );
  Term.Context := 'Aviation operational messages (SCR, MVT, RQP, CFM) for slot coordination, flight notifications, and client communication. Template-based generation with token replacement.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'Communication',
    'SlotCoordination',
    'DocumentManagement'
  );
  FTerms.Add('Mensajes', Term);

  // Minimas
  Term.SpanishTerm := 'Minimas';
  Term.EnglishTerms := TArray<string>.Create(
    'Minimum',
    'Min',
    'Lowest',
    'Floor'
  );
  Term.Context := 'Minimum production quantity required';
  Term.DomainTags := TArray<string>.Create(
    'ProductionPlanning',
    'Constraints',
    'Metrics'
  );
  FTerms.Add('Minimas', Term);

  // Modelo
  Term.SpanishTerm := 'Modelo';
  Term.EnglishTerms := TArray<string>.Create(
    'TaxForm',
    'FormModel',
    'TaxFormType',
    'DeclarationModel',
    'OfficialForm'
  );
  Term.Context := 'Official tax form type identified by number (e.g., Modelo 303 for quarterly VAT, Modelo 300 for annual VAT). Standardized format for tax authority.';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'SpanishTax',
    'Forms',
    'Regulatory'
  );
  FTerms.Add('Modelo', Term);

  // Modelos
  Term.SpanishTerm := 'Modelos';
  Term.EnglishTerms := TArray<string>.Create(
    'Models',
    'ProductModels',
    'Designs',
    'ProductDesigns',
    'PartNumbers'
  );
  Term.Context := 'Product models/designs specifying manufacturing parameters, materials, and production processes. Product master data for manufacturing.';
  Term.DomainTags := TArray<string>.Create(
    'Manufacturing',
    'ProductManagement',
    'MasterData',
    'Engineering'
  );
  FTerms.Add('Modelos', Term);

  // ============================================================
  // Category: MANUFACTURING & PRODUCTION (GMC)
  // ============================================================

  // Moldes
  Term.SpanishTerm := 'Moldes';
  Term.EnglishTerms := TArray<string>.Create(
    'Molds',
    'Tools',
    'ProductionMolds',
    'ManufacturingTools',
    'Dies'
  );
  Term.Context := 'Production molds/tooling for manufacturing operations including maintenance schedules, availability status, and measurement data. Critical manufacturing assets.';
  Term.DomainTags := TArray<string>.Create(
    'Manufacturing',
    'ToolManagement',
    'AssetManagement',
    'ProductionPlanning'
  );
  FTerms.Add('Moldes', Term);

  // Monopuesto
  Term.SpanishTerm := 'Monopuesto';
  Term.EnglishTerms := TArray<string>.Create(
    'SingleWorkstation',
    'Standalone',
    'SingleUser',
    'LocalMode'
  );
  Term.Context := 'Single-workstation installation mode for standalone operation without network connectivity.';
  Term.DomainTags := TArray<string>.Create(
    'Deployment',
    'StandaloneMode',
    'LocalInstallation',
    'Licensing'
  );
  FTerms.Add('Monopuesto', Term);

  // Movimientos
  Term.SpanishTerm := 'Movimientos';
  Term.EnglishTerms := TArray<string>.Create(
    'Movements',
    'FlightMovements',
    'AircraftMovements',
    'Operations',
    'FlightOperations'
  );
  Term.Context := 'Flight movement events tracking arrivals and departures. Used for operational reporting and regulatory compliance (e.g., AENA F1 export).';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'FlightOperations',
    'EventTracking',
    'RegulatoryCompliance'
  );
  FTerms.Add('Movimientos', Term);

  // Multipuesto
  Term.SpanishTerm := 'Multipuesto';
  Term.EnglishTerms := TArray<string>.Create(
    'MultiWorkstation',
    'MultiUser',
    'NetworkMode',
    'MultiStation'
  );
  Term.Context := 'Multi-workstation installation mode where multiple users access a shared server and database simultaneously.';
  Term.DomainTags := TArray<string>.Create(
    'Deployment',
    'MultiUser',
    'NetworkConfiguration',
    'Licensing'
  );
  FTerms.Add('Multipuesto', Term);

  // Navigator
  Term.SpanishTerm := 'Navigator';
  Term.EnglishTerms := TArray<string>.Create(
    'Navigator',
    'DataNavigator',
    'RecordNavigator',
    'DBNavigator'
  );
  Term.Context := 'Database navigator control with buttons for record navigation (first, prior, next, last) and editing (insert, delete, post, cancel).';
  Term.DomainTags := TArray<string>.Create(
    'UserInterface',
    'DataComponents',
    'RecordNavigation',
    'DataEditing'
  );
  FTerms.Add('Navigator', Term);

  // Nivel
  Term.SpanishTerm := 'Nivel';
  Term.EnglishTerms := TArray<string>.Create(
    'AccountLevel',
    'HierarchyLevel',
    'Level',
    'Depth',
    'AccountDepth'
  );
  Term.Context := 'Hierarchical level in the chart of accounts structure. Spanish accounts typically have 3-5 levels (e.g., 430 > 4300 > 43001).';
  Term.DomainTags := TArray<string>.Create(
    'ChartOfAccounts',
    'AccountingStructure',
    'Hierarchy'
  );
  FTerms.Add('Nivel', Term);

  // Nombre
  Term.SpanishTerm := 'Nombre';
  Term.EnglishTerms := TArray<string>.Create(
    'Name',
    'Description',
    'Title',
    'Label'
  );
  Term.Context := 'Name or title field. Descriptive name for entities and records.';
  Term.DomainTags := TArray<string>.Create(
    'DataFields',
    'Descriptors',
    'MasterData',
    'TextFields'
  );
  FTerms.Add('Nombre', Term);

  // Nomina
  Term.SpanishTerm := 'Nomina';
  Term.EnglishTerms := TArray<string>.Create(
    'Payroll',
    'PayrollCalculation',
    'WageCalculation',
    'Wages'
  );
  Term.Context := 'Payroll calculation (variant spelling without accent). Same as Nómina.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'LaborManagement',
    'Payroll',
    'HumanResources'
  );
  FTerms.Add('Nomina', Term);

  // ============================================================
  // PRODUCCIÓN MODULE GLOSSARY
  // ============================================================
  // ============================================================
  // Category: ORDER STATES
  // ============================================================

  // Nueva
  Term.SpanishTerm := 'Nueva';
  Term.EnglishTerms := TArray<string>.Create(
    'New',
    'NewOrder',
    'Fresh',
    'Initial'
  );
  Term.Context := 'Production order in initial state, not yet authorized for planning';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'OrderStates',
    'ProductionPlanning'
  );
  FTerms.Add('Nueva', Term);

  // NumSerie
  Term.SpanishTerm := 'NumSerie';
  Term.EnglishTerms := TArray<string>.Create(
    'SerialNumber',
    'SN',
    'UniqueIdentifier',
    'ItemSerial',
    'SerializedInventory',
    'UnitSerial'
  );
  Term.Context := 'Unique serial number for individual item tracking. Segmentation dimension for serialized inventory control and warranty management.';
  Term.DomainTags := TArray<string>.Create(
    'SerialTracking',
    'InventoryTracking',
    'AssetManagement',
    'WarrantyManagement'
  );
  FTerms.Add('NumSerie', Term);

  // Numero
  Term.SpanishTerm := 'Numero';
  Term.EnglishTerms := TArray<string>.Create(
    'Number',
    'DocumentNumber',
    'SequenceNumber',
    'ID',
    'Identifier'
  );
  Term.Context := 'A document number or identifier (variant spelling without accent). Same as Número.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'DataProperty',
    'Identification',
    'Numbering'
  );
  FTerms.Add('Numero', Term);

  // NumeroDeSerie
  Term.SpanishTerm := 'NumeroDeSerie';
  Term.EnglishTerms := TArray<string>.Create(
    'SerialNumber',
    'SN',
    'TrackingNumber',
    'UnitIdentifier'
  );
  Term.Context := 'Serial number with preposition';
  Term.DomainTags := TArray<string>.Create(
    'Traceability',
    'SerialNumbers'
  );
  FTerms.Add('NumeroDeSerie', Term);

  // NumeroSerie
  Term.SpanishTerm := 'NumeroSerie';
  Term.EnglishTerms := TArray<string>.Create(
    'SerialNumber',
    'SN',
    'TrackingNumber',
    'UnitIdentifier'
  );
  Term.Context := 'Individual serial number';
  Term.DomainTags := TArray<string>.Create(
    'Traceability',
    'SerialNumbers',
    'QualityControl'
  );
  FTerms.Add('NumeroSerie', Term);

  // NumerosSerie
  Term.SpanishTerm := 'NumerosSerie';
  Term.EnglishTerms := TArray<string>.Create(
    'SerialNumbers',
    'ProductSerialNumbers',
    'UnitIdentifiers',
    'TrackingNumbers',
    'ProductTracking',
    'SNs'
  );
  Term.Context := 'Serial number tracking for manufactured products enabling traceability, quality control, and warranty management. Critical for product genealogy.';
  Term.DomainTags := TArray<string>.Create(
    'Manufacturing',
    'Traceability',
    'QualityControl',
    'ProductTracking'
  );
  FTerms.Add('NumerosSerie', Term);

  // Nómina
  Term.SpanishTerm := 'Nómina';
  Term.EnglishTerms := TArray<string>.Create(
    'Payroll',
    'PayrollCalculation',
    'WageCalculation',
    'Wages',
    'PayStatement',
    'PayrollReport'
  );
  Term.Context := 'Payroll calculation for workers. The Proyectos module includes payroll calculation functionality to compute worker wages based on their time tracked in labor work reports.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'LaborManagement',
    'Payroll',
    'HumanResources'
  );
  FTerms.Add('Nómina', Term);

  // Nóminas
  Term.SpanishTerm := 'Nóminas';
  Term.EnglishTerms := TArray<string>.Create(
    'Payrolls',
    'PayrollCalculations',
    'WageCalculations',
    'PayStatements',
    'PayrollReports'
  );
  Term.Context := 'Multiple payroll calculations. Processing of worker wages across all projects and time periods.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'LaborManagement',
    'Payroll',
    'HumanResources'
  );
  FTerms.Add('Nóminas', Term);

  // Número
  Term.SpanishTerm := 'Número';
  Term.EnglishTerms := TArray<string>.Create(
    'Number',
    'DocumentNumber',
    'SequenceNumber',
    'ID',
    'Identifier',
    'RecordNumber'
  );
  Term.Context := 'A document number or identifier. Each work report has a series and number combination that uniquely identifies it.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'DataProperty',
    'Identification',
    'Numbering'
  );
  FTerms.Add('Número', Term);

  // Objetivo
  Term.SpanishTerm := 'Objetivo';
  Term.EnglishTerms := TArray<string>.Create(
    'Target',
    'Goal',
    'Objective',
    'Aim',
    'Standard'
  );
  Term.Context := 'Production target or objective to achieve';
  Term.DomainTags := TArray<string>.Create(
    'ProductionPlanning',
    'PerformanceMetrics',
    'Goals'
  );
  FTerms.Add('Objetivo', Term);

  // ============================================================
  // Category: RESOURCES
  // ============================================================

  // Operario
  Term.SpanishTerm := 'Operario';
  Term.EnglishTerms := TArray<string>.Create(
    'Worker',
    'Operator',
    'Laborer',
    'Employee',
    'Technician',
    'Craftsman'
  );
  Term.Context := 'A worker or operator who performs work on projects. Operarios are assigned to labor work reports (partes trabajo) and their time and costs are tracked per project.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'LaborManagement',
    'TimeTracking',
    'HumanResources'
  );
  FTerms.Add('Operario', Term);

  // Operarios
  Term.SpanishTerm := 'Operarios';
  Term.EnglishTerms := TArray<string>.Create(
    'Workers',
    'Operators',
    'Laborers',
    'Employees',
    'Technicians',
    'Craftsmen'
  );
  Term.Context := 'Multiple workers or operators. The workforce performing project work.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'LaborManagement',
    'TimeTracking',
    'HumanResources'
  );
  FTerms.Add('Operarios', Term);

  // OrdLins
  Term.SpanishTerm := 'OrdLins';
  Term.EnglishTerms := TArray<string>.Create(
    'OrderLines',
    'ProductionLines',
    'OperationLines',
    'WorkOrderLines'
  );
  Term.Context := 'Production order line details for individual operations and phases';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'ProcessControl',
    'Manufacturing'
  );
  FTerms.Add('OrdLins', Term);

  // OrdenClie
  Term.SpanishTerm := 'OrdenClie';
  Term.EnglishTerms := TArray<string>.Create(
    'CustomerOrders',
    'SalesOrders',
    'ClientOrders',
    'OrdersFromCustomers'
  );
  Term.Context := 'Customer sales orders linked to production orders';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'SalesOrders',
    'CustomerManagement'
  );
  FTerms.Add('OrdenClie', Term);

  // OrdenPeds
  Term.SpanishTerm := 'OrdenPeds';
  Term.EnglishTerms := TArray<string>.Create(
    'PurchaseOrders',
    'SupplierOrders',
    'ProcurementOrders',
    'OrdersToPurchase'
  );
  Term.Context := 'Purchase orders linked to production material requirements';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'Procurement',
    'MaterialPlanning'
  );
  FTerms.Add('OrdenPeds', Term);

  // Ordenacion
  Term.SpanishTerm := 'Ordenacion';
  Term.EnglishTerms := TArray<string>.Create(
    'Ordering',
    'Sorting',
    'RecordOrdering',
    'IndexOrdering'
  );
  Term.Context := 'Table ordering and sorting management. Controls record display order and index selection.';
  Term.DomainTags := TArray<string>.Create(
    'DatabaseAccess',
    'Sorting',
    'IndexManagement',
    'QueryOptimization'
  );
  FTerms.Add('Ordenacion', Term);

  // Ordenes
  Term.SpanishTerm := 'Ordenes';
  Term.EnglishTerms := TArray<string>.Create(
    'Orders',
    'WorkOrders',
    'ProductionOrders',
    'ManufacturingOrders',
    'JobOrders'
  );
  Term.Context := 'General orders including work orders, production orders, and service orders. Core transactional entity across multiple business contexts.';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'ProductionManagement',
    'WorkOrderManagement',
    'Operations'
  );
  FTerms.Add('Ordenes', Term);

  // OrdenesExterior
  Term.SpanishTerm := 'OrdenesExterior';
  Term.EnglishTerms := TArray<string>.Create(
    'ExternalOrders',
    'OutsideOrders',
    'SubcontractedWork',
    'ThirdPartyOrders'
  );
  Term.Context := 'Work orders for external/subcontracted billboard operations including third-party installations and services. Manages external vendor operations.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'VendorManagement',
    'Subcontracting',
    'FieldOperations'
  );
  FTerms.Add('OrdenesExterior', Term);

  // OrigenID
  Term.SpanishTerm := 'OrigenID';
  Term.EnglishTerms := TArray<string>.Create(
    'SourceID',
    'OriginID',
    'DocumentID',
    'RecordIdentifier',
    'SourceRecordID',
    'LinkedDocumentID'
  );
  Term.Context := 'Source record identifier (usually RecordIdent). Links events to originating business document for traceability and event ownership.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'AuditTrail',
    'DocumentTracking',
    'ReferentialIntegrity'
  );
  FTerms.Add('OrigenID', Term);

  // OrigenTB
  Term.SpanishTerm := 'OrigenTB';
  Term.EnglishTerms := TArray<string>.Create(
    'SourceTable',
    'OriginTable',
    'SourceIdentifier',
    'DocumentType',
    'SourceType',
    'OriginContext'
  );
  Term.Context := 'String identifier for event source context (not table name!). Links events to business documents like FACTURA_VENTA, ALBARAN_COMPRA. Defined by business logic.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'AuditTrail',
    'DocumentTracking',
    'Traceability'
  );
  FTerms.Add('OrigenTB', Term);

  // PEDCLI
  Term.SpanishTerm := 'PEDCLI';
  Term.EnglishTerms := TArray<string>.Create(
    'CustomerOrders',
    'SalesOrders',
    'PendingCustomerOrders',
    'ClientOrders',
    'CustomerOrderQuantity',
    'SalesBacklog'
  );
  Term.Context := 'Pending customer order quantity data class (Pedido Cliente). Tracks outstanding customer orders not yet fulfilled. Incrementable (+/-) class.';
  Term.DomainTags := TArray<string>.Create(
    'SalesOrders',
    'OrderManagement',
    'CustomerManagement',
    'DemandTracking'
  );
  FTerms.Add('PEDCLI', Term);

  // PEDMIN
  Term.SpanishTerm := 'PEDMIN';
  Term.EnglishTerms := TArray<string>.Create(
    'MinimumOrder',
    'MOQ',
    'MinimumOrderQuantity',
    'OrderMinimum',
    'MinOrderQty',
    'MinimumPurchase'
  );
  Term.Context := 'Minimum order quantity for purchasing. Defines smallest quantity that can be ordered from supplier. Non-incrementable (=) class.';
  Term.DomainTags := TArray<string>.Create(
    'Procurement',
    'OrderManagement',
    'PurchaseOrders',
    'SupplierManagement'
  );
  FTerms.Add('PEDMIN', Term);

  // PEDPRV
  Term.SpanishTerm := 'PEDPRV';
  Term.EnglishTerms := TArray<string>.Create(
    'SupplierOrders',
    'PurchaseOrders',
    'PendingSupplierOrders',
    'VendorOrders',
    'POQuantity',
    'OnOrder'
  );
  Term.Context := 'Pending supplier order quantity data class (Pedido Proveedor). Tracks outstanding purchase orders from suppliers. Incrementable (+/-) class.';
  Term.DomainTags := TArray<string>.Create(
    'PurchaseOrders',
    'SupplierManagement',
    'Procurement',
    'SupplyTracking'
  );
  FTerms.Add('PEDPRV', Term);

  // PESO
  Term.SpanishTerm := 'PESO';
  Term.EnglishTerms := TArray<string>.Create(
    'Weight',
    'ProductWeight',
    'ItemWeight',
    'Mass',
    'UnitWeight',
    'Kilos'
  );
  Term.Context := 'Product weight specification for shipping and logistics calculations. Non-incrementable (=) class.';
  Term.DomainTags := TArray<string>.Create(
    'ProductSpecifications',
    'ShippingManagement',
    'Logistics',
    'ProductManagement'
  );
  FTerms.Add('PESO', Term);

  // PGC
  Term.SpanishTerm := 'PGC';
  Term.EnglishTerms := TArray<string>.Create(
    'ChartOfAccounts',
    'GeneralAccountingPlan',
    'AccountPlan',
    'SpanishChartOfAccounts',
    'StandardChartOfAccounts'
  );
  Term.Context := 'Plan General Contable - Spanish standard chart of accounts. Official accounting framework mandated by Spanish law.';
  Term.DomainTags := TArray<string>.Create(
    'ChartOfAccounts',
    'Accounting',
    'SpanishCompliance',
    'StandardsCompliance'
  );
  FTerms.Add('PGC', Term);

  // PVP
  Term.SpanishTerm := 'PVP';
  Term.EnglishTerms := TArray<string>.Create(
    'RetailPrice',
    'PublicPrice',
    'SalesPrice',
    'ListPrice',
    'CustomerPrice',
    'SellingPrice'
  );
  Term.Context := 'Retail/public sales price data class (Precio Venta Público). Non-incrementable (=) class, can vary by warehouse location.';
  Term.DomainTags := TArray<string>.Create(
    'Pricing',
    'SalesManagement',
    'ProductManagement',
    'PriceManagement'
  );
  FTerms.Add('PVP', Term);

  // Pagaré
  Term.SpanishTerm := 'Pagaré';
  Term.EnglishTerms := TArray<string>.Create(
    'PromissoryNote',
    'IOU',
    'PaymentNote',
    'DebtNote',
    'PaymentPromise'
  );
  Term.Context := 'Promissory note document. Written promise to pay specific amount on specific date. Can be printed and physically delivered.';
  Term.DomainTags := TArray<string>.Create(
    'BankingOperations',
    'AccountsReceivable',
    'Collections',
    'Documents'
  );
  FTerms.Add('Pagaré', Term);

  // Pagarés
  Term.SpanishTerm := 'Pagarés';
  Term.EnglishTerms := TArray<string>.Create(
    'PromissoryNotes',
    'IOUs',
    'PaymentNotes',
    'DebtNotes',
    'PaymentPromises'
  );
  Term.Context := 'Multiple promissory note documents. Batch printed or managed for payment processing.';
  Term.DomainTags := TArray<string>.Create(
    'BankingOperations',
    'AccountsReceivable',
    'Collections'
  );
  FTerms.Add('Pagarés', Term);

  // Pago
  Term.SpanishTerm := 'Pago';
  Term.EnglishTerms := TArray<string>.Create(
    'Payment',
    'Disbursement',
    'PaymentTransaction',
    'CashPayment'
  );
  Term.Context := 'Payment to supplier or creditor. Outgoing payment transaction.';
  Term.DomainTags := TArray<string>.Create(
    'AccountsPayable',
    'PaymentProcessing',
    'CashManagement',
    'Disbursements'
  );
  FTerms.Add('Pago', Term);

  // Pagos
  Term.SpanishTerm := 'Pagos';
  Term.EnglishTerms := TArray<string>.Create(
    'Payments',
    'Disbursements',
    'PaymentTransactions',
    'CashPayments'
  );
  Term.Context := 'Plural form. Multiple payment transactions.';
  Term.DomainTags := TArray<string>.Create(
    'AccountsPayable',
    'PaymentProcessing',
    'CashManagement',
    'Disbursements'
  );
  FTerms.Add('Pagos', Term);

  // Palet
  Term.SpanishTerm := 'Palet';
  Term.EnglishTerms := TArray<string>.Create(
    'Pallet',
    'Skid',
    'Platform',
    'ShippingUnit'
  );
  Term.Context := 'Individual pallet for shipping';
  Term.DomainTags := TArray<string>.Create(
    'Logistics',
    'WarehouseManagement',
    'Shipping'
  );
  FTerms.Add('Palet', Term);

  // ============================================================
  // Category: LOGISTICS
  // ============================================================

  // Palets
  Term.SpanishTerm := 'Palets';
  Term.EnglishTerms := TArray<string>.Create(
    'Pallets',
    'Skids',
    'Platforms',
    'ShippingUnits'
  );
  Term.Context := 'Pallet management for logistics and warehouse operations';
  Term.DomainTags := TArray<string>.Create(
    'Logistics',
    'WarehouseManagement',
    'Shipping',
    'Packaging'
  );
  FTerms.Add('Palets', Term);

  // Parking
  Term.SpanishTerm := 'Parking';
  Term.EnglishTerms := TArray<string>.Create(
    'Parking',
    'AircraftParking',
    'ParkingPositions',
    'Apron',
    'ParkingStands'
  );
  Term.Context := 'Aircraft parking positions at airports including stand numbers, sizes, and availability. Essential for ground operations planning.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'GroundOperations',
    'Infrastructure',
    'ResourceManagement'
  );
  FTerms.Add('Parking', Term);

  // ============================================================
  // Category: WORK ORDERS & REPORTS
  // ============================================================

  // Parte
  Term.SpanishTerm := 'Parte';
  Term.EnglishTerms := TArray<string>.Create(
    'WorkOrder',
    'WorkReport',
    'JobTicket',
    'WorkTicket',
    'TimeSheet',
    'Report'
  );
  Term.Context := 'A work report or order that records work performed on a project. There are three types: materials (MATE), labor (TRAB), and expenses (GAS). Each parte records what was done, by whom, on which project section, and at what cost.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'TimeTracking',
    'JobCosting',
    'LaborManagement'
  );
  FTerms.Add('Parte', Term);

  // Partes
  Term.SpanishTerm := 'Partes';
  Term.EnglishTerms := TArray<string>.Create(
    'WorkOrders',
    'ServiceReports',
    'MaintenanceOrders',
    'JobTickets',
    'FieldReports',
    'WorkReports'
  );
  Term.Context := 'Work orders for billboard maintenance, installation, and service operations. Tracks field operations, labor, materials, and completion status.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'FieldOperations',
    'MaintenanceManagement',
    'WorkOrderManagement'
  );
  FTerms.Add('Partes', Term);

  // Patrocinadores
  Term.SpanishTerm := 'Patrocinadores';
  Term.EnglishTerms := TArray<string>.Create(
    'Sponsors',
    'SponsorCompanies',
    'Patrons',
    'CorporateSponsors'
  );
  Term.Context := 'Sponsor companies for advertising campaigns and events. Special customer category with distinct contract and billing arrangements.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'CustomerManagement',
    'Sponsorship',
    'EventManagement'
  );
  FTerms.Add('Patrocinadores', Term);

  // Pedido
  Term.SpanishTerm := 'Pedido';
  Term.EnglishTerms := TArray<string>.Create(
    'Order',
    'PurchaseOrder',
    'SalesOrder',
    'CustomerOrder'
  );
  Term.Context := 'Sales or purchase order document. Used for both customer orders (sales) and supplier orders (procurement).';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'SalesOrders',
    'PurchaseOrders',
    'Procurement'
  );
  FTerms.Add('Pedido', Term);

  // Pedidos
  Term.SpanishTerm := 'Pedidos';
  Term.EnglishTerms := TArray<string>.Create(
    'Orders',
    'PurchaseOrders',
    'SalesOrders',
    'CustomerOrders',
    'CustomerOrders',
    'Requisitions'
  );
  Term.Context := 'Plural form. Multiple order documents.';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'SalesOrders',
    'PurchaseOrders',
    'Procurement'
  );
  FTerms.Add('Pedidos', Term);

  // Peticion
  Term.SpanishTerm := 'Peticion';
  Term.EnglishTerms := TArray<string>.Create(
    'Request',
    'ServerRequest',
    'ServiceRequest',
    'Command'
  );
  Term.Context := 'Network request sent from client to server for data retrieval, command execution, or service invocation.';
  Term.DomainTags := TArray<string>.Create(
    'NetworkCommunication',
    'ClientServer',
    'RequestResponse',
    'Communication'
  );
  FTerms.Add('Peticion', Term);

  // Peticiones
  Term.SpanishTerm := 'Peticiones';
  Term.EnglishTerms := TArray<string>.Create(
    'Requests',
    'ServerRequests',
    'ServiceRequests',
    'Commands'
  );
  Term.Context := 'Multiple network requests or request handling system for client-server communication.';
  Term.DomainTags := TArray<string>.Create(
    'NetworkCommunication',
    'ClientServer',
    'RequestResponse'
  );
  FTerms.Add('Peticiones', Term);

  // ============================================================
  // Category: PRODUCTION METRICS
  // ============================================================

  // Piezas
  Term.SpanishTerm := 'Piezas';
  Term.EnglishTerms := TArray<string>.Create(
    'Pieces',
    'Units',
    'Items',
    'Quantity',
    'Count'
  );
  Term.Context := 'Production quantity measured in pieces or units';
  Term.DomainTags := TArray<string>.Create(
    'ProductionTracking',
    'Metrics',
    'Quantity',
    'Manufacturing'
  );
  FTerms.Add('Piezas', Term);

  // Planificaciones
  Term.SpanishTerm := 'Planificaciones';
  Term.EnglishTerms := TArray<string>.Create(
    'Planning',
    'ProductionSchedule',
    'ProductionPlanning',
    'ManufacturingSchedule',
    'Scheduling'
  );
  Term.Context := 'Production planning and scheduling including work orders, resource allocation, and timeline management. Core MES functionality for shop floor control.';
  Term.DomainTags := TArray<string>.Create(
    'Manufacturing',
    'ProductionPlanning',
    'MES',
    'Scheduling'
  );
  FTerms.Add('Planificaciones', Term);

  // Planificada
  Term.SpanishTerm := 'Planificada';
  Term.EnglishTerms := TArray<string>.Create(
    'Planned',
    'Scheduled',
    'Allocated',
    'ResourcePlanned'
  );
  Term.Context := 'Production order with resources and schedule planned';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'OrderStates',
    'ProductionPlanning',
    'CapacityPlanning'
  );
  FTerms.Add('Planificada', Term);

  // ============================================================
  // Category: TEMPLATES & AUTOMATION
  // ============================================================

  // Plantilla
  Term.SpanishTerm := 'Plantilla';
  Term.EnglishTerms := TArray<string>.Create(
    'Template',
    'JournalTemplate',
    'EntryTemplate',
    'Pattern',
    'Blueprint',
    'DocumentTemplate'
  );
  Term.Context := 'Journal entry template for automated posting. Pre-configured entries with variable fields (accounts, amounts, dates) filled at runtime.';
  Term.DomainTags := TArray<string>.Create(
    'Automation',
    'Templates',
    'JournalEntries',
    'Productivity'
  );
  FTerms.Add('Plantilla', Term);

  // Plantillas
  Term.SpanishTerm := 'Plantillas';
  Term.EnglishTerms := TArray<string>.Create(
    'Templates',
    'JournalTemplates',
    'EntryTemplates',
    'Patterns',
    'Blueprints',
    'DocumentTemplates'
  );
  Term.Context := 'Multiple journal entry templates. Library of reusable posting patterns for common transactions.';
  Term.DomainTags := TArray<string>.Create(
    'Automation',
    'Templates',
    'JournalEntries',
    'Productivity'
  );
  FTerms.Add('Plantillas', Term);

  // PlantillasMensajes
  Term.SpanishTerm := 'PlantillasMensajes';
  Term.EnglishTerms := TArray<string>.Create(
    'MessageTemplates',
    'Templates',
    'MessagePatterns',
    'CommunicationTemplates'
  );
  Term.Context := 'Message templates for aviation communication with token-based dynamic content generation. Supports 83+ tokens for flight, aircraft, and operational data.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'Communication',
    'TemplateManagement',
    'Automation'
  );
  FTerms.Add('PlantillasMensajes', Term);

  // ============================================================
  // Category: AVIATION
  Term.SpanishTerm := 'Plaza';
  Term.EnglishTerms := TArray<string>.Create(
    'Location',
    'Site',
    'Base',
    'OperationalBase',
    'FBOlocation',
    'Station'
  );
  Term.Context := 'FBO operational location/site where services are provided. Multi-airport operations use different plazas for operational segregation and reporting.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'FBOoperations',
    'OrganizationalStructure',
    'MultiSiteOperations'
  );
  FTerms.Add('Plaza', Term);

  // Poblaciones
  Term.SpanishTerm := 'Poblaciones';
  Term.EnglishTerms := TArray<string>.Create(
    'Cities',
    'Towns',
    'Municipalities',
    'Locations',
    'GeographicAreas'
  );
  Term.Context := 'Cities/towns where billboards are located. Geographic classification for campaign planning, pricing, and market analysis.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'Geography',
    'MarketSegmentation',
    'LocationManagement'
  );
  FTerms.Add('Poblaciones', Term);

  // PorEmail
  Term.SpanishTerm := 'PorEmail';
  Term.EnglishTerms := TArray<string>.Create(
    'ByEmail',
    'ViaEmail',
    'EmailDelivery',
    'ElectronicMail'
  );
  Term.Context := 'Email sending functionality. Framework for composing and sending emails from application.';
  Term.DomainTags := TArray<string>.Create(
    'Email',
    'Communication',
    'Messaging'
  );
  FTerms.Add('PorEmail', Term);

  // Precio
  Term.SpanishTerm := 'Precio';
  Term.EnglishTerms := TArray<string>.Create(
    'Price',
    'UnitPrice',
    'SalesPrice',
    'ListPrice',
    'ItemPrice',
    'SellPrice'
  );
  Term.Context := 'Product or service price. Base price before discounts and taxes.';
  Term.DomainTags := TArray<string>.Create(
    'Pricing',
    'SalesManagement',
    'PriceManagement',
    'ProductPricing'
  );
  FTerms.Add('Precio', Term);

  // Precios
  Term.SpanishTerm := 'Precios';
  Term.EnglishTerms := TArray<string>.Create(
    'Prices',
    'UnitPrices',
    'SalesPrices',
    'ListPrices',
    'Pricing',
    'PriceLists'
  );
  Term.Context := 'Plural form. Multiple price points and pricing tiers.';
  Term.DomainTags := TArray<string>.Create(
    'Pricing',
    'SalesManagement',
    'PriceManagement',
    'ProductPricing'
  );
  FTerms.Add('Precios', Term);

  // ============================================================
  // Category: BUDGETS & FORECASTING
  // ============================================================

  // Presupuesto
  Term.SpanishTerm := 'Presupuesto';
  Term.EnglishTerms := TArray<string>.Create(
    'Budget',
    'BudgetPlan',
    'FinancialPlan',
    'ForecastBudget',
    'AnnualBudget',
    'Quote'
  );
  Term.Context := 'Budget or financial plan. Planned amounts by account and period for comparison against actuals.';
  Term.DomainTags := TArray<string>.Create(
    'Budgeting',
    'Planning',
    'Management',
    'FinancialReporting'
  );
  FTerms.Add('Presupuesto', Term);

  // Presupuestos
  Term.SpanishTerm := 'Presupuestos';
  Term.EnglishTerms := TArray<string>.Create(
    'Budgets',
    'BudgetPlans',
    'FinancialPlans',
    'ForecastBudgets',
    'AnnualBudgets',
    'Quotes'
  );
  Term.Context := 'Multiple budgets or financial plans. Companies may maintain multiple budget versions or scenarios.';
  Term.DomainTags := TArray<string>.Create(
    'Budgeting',
    'Planning',
    'Management',
    'Quotations'
  );
  FTerms.Add('Presupuestos', Term);

  // Previsión
  Term.SpanishTerm := 'Previsión';
  Term.EnglishTerms := TArray<string>.Create(
    'Forecast',
    'Projection',
    'Prediction',
    'CashFlowForecast',
    'FinancialForecast'
  );
  Term.Context := 'Forecast or projection. Used for cash flow forecasting based on due dates and expected collections/payments.';
  Term.DomainTags := TArray<string>.Create(
    'Forecasting',
    'CashManagement',
    'Planning',
    'Treasury'
  );
  FTerms.Add('Previsión', Term);

  // Prevista
  Term.SpanishTerm := 'Prevista';
  Term.EnglishTerms := TArray<string>.Create(
    'Forecast',
    'Planned',
    'Expected',
    'Estimated'
  );
  Term.Context := 'Forecasted or planned (feminine form)';
  Term.DomainTags := TArray<string>.Create(
    'ProductionPlanning',
    'Forecasting'
  );
  FTerms.Add('Prevista', Term);

  // Previstas
  Term.SpanishTerm := 'Previstas';
  Term.EnglishTerms := TArray<string>.Create(
    'Forecast',
    'Planned',
    'Expected',
    'Estimated'
  );
  Term.Context := 'Forecasted or planned (feminine plural)';
  Term.DomainTags := TArray<string>.Create(
    'ProductionPlanning',
    'Forecasting'
  );
  FTerms.Add('Previstas', Term);

  // Previsto
  Term.SpanishTerm := 'Previsto';
  Term.EnglishTerms := TArray<string>.Create(
    'Forecast',
    'Planned',
    'Expected',
    'Estimated',
    'Projected'
  );
  Term.Context := 'Forecasted or planned production quantity/time';
  Term.DomainTags := TArray<string>.Create(
    'ProductionPlanning',
    'Forecasting',
    'Metrics'
  );
  FTerms.Add('Previsto', Term);

  // Prospectos
  Term.SpanishTerm := 'Prospectos';
  Term.EnglishTerms := TArray<string>.Create(
    'Prospects',
    'Leads',
    'PotentialClients',
    'SalesLeads',
    'ProspectiveAdvertisers'
  );
  Term.Context := 'Sales prospects and leads for new advertiser acquisition. CRM entity for pipeline management and lead conversion tracking.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'CRM',
    'LeadManagement',
    'SalesPipeline'
  );
  FTerms.Add('Prospectos', Term);

  // Proteccion
  Term.SpanishTerm := 'Proteccion';
  Term.EnglishTerms := TArray<string>.Create(
    'Protection',
    'CopyProtection',
    'LicenseProtection',
    'Security'
  );
  Term.Context := 'Software copy protection and licensing enforcement system. Prevents unauthorized use and tracks user licenses.';
  Term.DomainTags := TArray<string>.Create(
    'CopyProtection',
    'Licensing',
    'Security',
    'AntiPiracy'
  );
  FTerms.Add('Proteccion', Term);

  // Proveedores
  Term.SpanishTerm := 'Proveedores';
  Term.EnglishTerms := TArray<string>.Create(
    'Suppliers',
    'Vendors',
    'Providers',
    'Sellers',
    'ServiceProviders',
    'SupplierAccounts'
  );
  Term.Context := 'Plural form. Multiple supplier entities.';
  Term.DomainTags := TArray<string>.Create(
    'SupplierManagement',
    'Procurement',
    'PurchaseOrders',
    'AccountsPayable'
  );
  FTerms.Add('Proveedores', Term);

  // ============================================================
  // PROYECTOS MODULE GLOSSARY
  // ============================================================
  // ============================================================
  // Category: PROJECT MANAGEMENT
  // ============================================================

  // Proyecto
  Term.SpanishTerm := 'Proyecto';
  Term.EnglishTerms := TArray<string>.Create(
    'Project',
    'Job',
    'ProjectManagement',
    'ProjectTracking',
    'JobTracking',
    'ProjectRecord'
  );
  Term.Context := 'A project or job in the system. Used to track all work, costs, and budgets for a customer project. Projects have sections, budgets, and work reports (partes) associated with them.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'JobCosting',
    'ProjectTracking',
    'BusinessEntity'
  );
  FTerms.Add('Proyecto', Term);

  // Proyectos
  Term.SpanishTerm := 'Proyectos';
  Term.EnglishTerms := TArray<string>.Create(
    'Projects',
    'Jobs',
    'ProjectList',
    'ProjectManagement',
    'JobList'
  );
  Term.Context := 'Multiple projects or the projects module. Used to manage all customer projects, track progress, costs, and profitability across multiple jobs.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'JobCosting',
    'ProjectTracking',
    'BusinessEntity'
  );
  FTerms.Add('Proyectos', Term);

  // Punteo
  Term.SpanishTerm := 'Punteo';
  Term.EnglishTerms := TArray<string>.Create(
    'Reconciliation',
    'Checkmark',
    'TickOff',
    'Mark',
    'ReconciliationMark'
  );
  Term.Context := 'Marking or ticking off transactions during reconciliation process. Used in bank reconciliation and account matching.';
  Term.DomainTags := TArray<string>.Create(
    'Reconciliation',
    'BankingOperations',
    'AccountingProcess'
  );
  FTerms.Add('Punteo', Term);

  // RESERVA
  Term.SpanishTerm := 'RESERVA';
  Term.EnglishTerms := TArray<string>.Create(
    'Reserved',
    'Reservation',
    'ReservedStock',
    'AllocatedStock',
    'CommittedInventory',
    'StockReservation'
  );
  Term.Context := 'Reserved inventory quantity data class. Stock allocated to orders but not yet shipped. Incrementable (+/-) class.';
  Term.DomainTags := TArray<string>.Create(
    'InventoryTracking',
    'StockControl',
    'OrderManagement',
    'AllocationManagement'
  );
  FTerms.Add('RESERVA', Term);

  // Tripulante (singular)
  Term.SpanishTerm := 'Tripulante';
  Term.EnglishTerms := TArray<string>.Create(
    'CrewMember',
    'FlightCrewMember',
    'Pilot',
    'CabinCrewMember'
  );
  Term.Context := 'Individual flight crew member with rank, certifications, and accommodation preferences.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'CrewManagement',
    'HumanResources'
  );
  FTerms.Add('Tripulante', Term);

  // Rangos
  Term.SpanishTerm := 'Rangos';
  Term.EnglishTerms := TArray<string>.Create(
    'Ranks',
    'CrewRanks',
    'CrewPositions',
    'CrewGrades',
    'FlightCrewRanks'
  );
  Term.Context := 'Crew rank classifications (Captain, First Officer, Purser, etc.) for organizational hierarchy and service planning.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'CrewManagement',
    'Classification',
    'HumanResources'
  );
  FTerms.Add('Rangos', Term);

  // Rappel
  Term.SpanishTerm := 'Rappel';
  Term.EnglishTerms := TArray<string>.Create(
    'Rebate',
    'VolumeDiscount',
    'VolumeRebate',
    'RetroactiveDiscount'
  );
  Term.Context := 'Customer rebate based on volume or turnover. Retroactive discount applied when purchase targets are met.';
  Term.DomainTags := TArray<string>.Create(
    'Rebates',
    'CustomerIncentives',
    'VolumeDiscounts',
    'PriceManagement'
  );
  FTerms.Add('Rappel', Term);

  // Rappels
  Term.SpanishTerm := 'Rappels';
  Term.EnglishTerms := TArray<string>.Create(
    'Rebates',
    'VolumeDiscounts',
    'VolumeRebates',
    'RetroactiveDiscounts'
  );
  Term.Context := 'Plural form. Multiple rebate programs and structures.';
  Term.DomainTags := TArray<string>.Create(
    'Rebates',
    'CustomerIncentives',
    'VolumeDiscounts',
    'PriceManagement'
  );
  FTerms.Add('Rappels', Term);

  // Realizada
  Term.SpanishTerm := 'Realizada';
  Term.EnglishTerms := TArray<string>.Create(
    'Actual',
    'Completed',
    'Achieved',
    'Realized'
  );
  Term.Context := 'Actual production completed (feminine form)';
  Term.DomainTags := TArray<string>.Create(
    'ProductionTracking',
    'Metrics',
    'Actuals'
  );
  FTerms.Add('Realizada', Term);

  // Realizadas
  Term.SpanishTerm := 'Realizadas';
  Term.EnglishTerms := TArray<string>.Create(
    'Actual',
    'Completed',
    'Achieved',
    'Realized'
  );
  Term.Context := 'Actual production completed (feminine plural)';
  Term.DomainTags := TArray<string>.Create(
    'ProductionTracking',
    'Metrics',
    'Actuals'
  );
  FTerms.Add('Realizadas', Term);

  // Realizado
  Term.SpanishTerm := 'Realizado';
  Term.EnglishTerms := TArray<string>.Create(
    'Actual',
    'Completed',
    'Achieved',
    'Realized',
    'Executed'
  );
  Term.Context := 'Actual production completed (masculine form)';
  Term.DomainTags := TArray<string>.Create(
    'ProductionTracking',
    'Metrics',
    'Actuals'
  );
  FTerms.Add('Realizado', Term);

  // Reasignar
  Term.SpanishTerm := 'Reasignar';
  Term.EnglishTerms := TArray<string>.Create(
    'Reassign',
    'Reallocate',
    'Redistribute',
    'Transfer',
    'Move'
  );
  Term.Context := 'Reassign resources, serial numbers, or production assignments';
  Term.DomainTags := TArray<string>.Create(
    'ResourcePlanning',
    'OrderManagement',
    'Traceability'
  );
  FTerms.Add('Reasignar', Term);

  // Recalcular
  Term.SpanishTerm := 'Recalcular';
  Term.EnglishTerms := TArray<string>.Create(
    'Recalculate',
    'Recompute',
    'Refresh',
    'Update',
    'Reevaluate',
    'Rebuild'
  );
  Term.Context := 'Recalculate production costs, times, or requirements after changes';
  Term.DomainTags := TArray<string>.Create(
    'Calculations',
    'CostAccounting',
    'OrderManagement',
    'DataMaintenance'
  );
  FTerms.Add('Recalcular', Term);

  // Recargo
  Term.SpanishTerm := 'Recargo';
  Term.EnglishTerms := TArray<string>.Create(
    'Surcharge',
    'EquivalenceSurcharge',
    'AdditionalTax',
    'TaxSurcharge'
  );
  Term.Context := 'Spanish equivalence surcharge tax (Recargo de Equivalencia). Special VAT regime for small retailers.';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'SpanishTax',
    'VATRegimes',
    'RetailTax'
  );
  FTerms.Add('Recargo', Term);

  // ============================================================
  // Category: PRODUCT FEATURES & IDENTIFICATION
  // ============================================================

  // Referencia
  Term.SpanishTerm := 'Referencia';
  Term.EnglishTerms := TArray<string>.Create(
    'Reference',
    'SKU',
    'ProductCode',
    'ItemNumber',
    'PartNumber',
    'ItemCode'
  );
  Term.Context := 'Product reference code or SKU. Unique identifier for articles in the catalog.';
  Term.DomainTags := TArray<string>.Create(
    'ProductManagement',
    'CatalogManagement',
    'Inventory',
    'ItemMaster'
  );
  FTerms.Add('Referencia', Term);

  // Referencias
  Term.SpanishTerm := 'Referencias';
  Term.EnglishTerms := TArray<string>.Create(
    'References',
    'SKUs',
    'ProductCodes',
    'ItemNumbers',
    'PartNumbers',
    'ItemCodes'
  );
  Term.Context := 'Plural form. Multiple product reference codes.';
  Term.DomainTags := TArray<string>.Create(
    'ProductManagement',
    'CatalogManagement',
    'Inventory',
    'ItemMaster'
  );
  FTerms.Add('Referencias', Term);

  // Regenerar
  Term.SpanishTerm := 'Regenerar';
  Term.EnglishTerms := TArray<string>.Create(
    'Regenerate',
    'Rebuild',
    'Recreate',
    'Refresh',
    'Reconstruct'
  );
  Term.Context := 'Regenerate production data, serial numbers, or system records';
  Term.DomainTags := TArray<string>.Create(
    'DataManagement',
    'Maintenance',
    'OrderManagement'
  );
  FTerms.Add('Regenerar', Term);

  // ============================================================
  // Category: BANKING & PAYMENTS
  // ============================================================

  // Remesa
  Term.SpanishTerm := 'Remesa';
  Term.EnglishTerms := TArray<string>.Create(
    'Remittance',
    'PaymentBatch',
    'BankRemittance',
    'BatchPayment',
    'CollectionBatch',
    'BankCollection'
  );
  Term.Context := 'Bank remittance or payment batch. Groups multiple bills/invoices for electronic submission to bank (SEPA or Spanish norms CSB 19, 32, 58).';
  Term.DomainTags := TArray<string>.Create(
    'BankingOperations',
    'PaymentProcessing',
    'SEPA',
    'Collections'
  );
  FTerms.Add('Remesa', Term);

  // Remesas
  Term.SpanishTerm := 'Remesas';
  Term.EnglishTerms := TArray<string>.Create(
    'Remittances',
    'PaymentBatches',
    'BankRemittances',
    'BatchPayments',
    'CollectionBatches',
    'BankCollections'
  );
  Term.Context := 'Multiple bank remittances or payment batches. Used for managing and tracking multiple banking operations.';
  Term.DomainTags := TArray<string>.Create(
    'BankingOperations',
    'PaymentProcessing',
    'SEPA',
    'DirectDebit'
  );
  FTerms.Add('Remesas', Term);

  // Rentacar
  Term.SpanishTerm := 'Rentacar';
  Term.EnglishTerms := TArray<string>.Create(
    'CarRentalCompany',
    'RentalCompany',
    'VehicleRentalProvider',
    'CarRentalProvider'
  );
  Term.Context := 'Car rental company master data including contact information, rates, and contracts for crew transportation services.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'CrewServices',
    'MasterData',
    'SupplierManagement'
  );
  FTerms.Add('Rentacar', Term);

  // Reparto
  Term.SpanishTerm := 'Reparto';
  Term.EnglishTerms := TArray<string>.Create(
    'Delivery',
    'Distribution',
    'Dispatch',
    'Shipment'
  );
  Term.Context := 'Delivery or distribution operation. Physical delivery of goods to customers.';
  Term.DomainTags := TArray<string>.Create(
    'DeliveryManagement',
    'Logistics',
    'Distribution',
    'Fulfillment'
  );
  FTerms.Add('Reparto', Term);

  // Repostaje
  Term.SpanishTerm := 'Repostaje';
  Term.EnglishTerms := TArray<string>.Create(
    'Refueling',
    'FuelService',
    'AircraftRefueling',
    'FuelingService',
    'Fueling'
  );
  Term.Context := 'Aircraft refueling records including quantity (liters), fuel card, cost, and invoice details. Critical for flight operations and cost tracking.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'FuelManagement',
    'GroundServices',
    'CostTracking'
  );
  FTerms.Add('Repostaje', Term);

  // Revalorar
  Term.SpanishTerm := 'Revalorar';
  Term.EnglishTerms := TArray<string>.Create(
    'Revalue',
    'Revaluate',
    'Reprice',
    'Reassess',
    'UpdateValue'
  );
  Term.Context := 'Revalue production costs using updated prices';
  Term.DomainTags := TArray<string>.Create(
    'CostAccounting',
    'Finance',
    'OrderManagement'
  );
  FTerms.Add('Revalorar', Term);

  // Revisar
  Term.SpanishTerm := 'Revisar';
  Term.EnglishTerms := TArray<string>.Create(
    'Review',
    'Check',
    'Verify',
    'Inspect',
    'Audit',
    'Validate'
  );
  Term.Context := 'Review and validate production orders and data';
  Term.DomainTags := TArray<string>.Create(
    'QualityControl',
    'Verification',
    'OrderManagement',
    'DataIntegrity'
  );
  FTerms.Add('Revisar', Term);

  // Rotaciones
  Term.SpanishTerm := 'Rotaciones';
  Term.EnglishTerms := TArray<string>.Create(
    'Rotations',
    'AircraftRotations',
    'MultiLegJourneys',
    'FlightRotations',
    'TurnArounds'
  );
  Term.Context := 'Aircraft rotations tracking multi-leg journeys for operational planning and crew scheduling. Links multiple flights into logical trip sequences.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'FlightOperations',
    'OperationalPlanning',
    'CrewScheduling'
  );
  FTerms.Add('Rotaciones', Term);

  // Rotura
  Term.SpanishTerm := 'Rotura';
  Term.EnglishTerms := TArray<string>.Create(
    'Scrap',
    'Breakage',
    'Defect',
    'Reject'
  );
  Term.Context := 'Scrap or defective unit';
  Term.DomainTags := TArray<string>.Create(
    'QualityControl',
    'ProductionTracking'
  );
  FTerms.Add('Rotura', Term);

  // Roturas
  Term.SpanishTerm := 'Roturas';
  Term.EnglishTerms := TArray<string>.Create(
    'Scrap',
    'Breakage',
    'Waste',
    'Defects',
    'Rejects'
  );
  Term.Context := 'Scrap and defective units in production process';
  Term.DomainTags := TArray<string>.Create(
    'QualityControl',
    'ProductionTracking',
    'WasteManagement'
  );
  FTerms.Add('Roturas', Term);

  // ============================================================
  // Category: SHIPPING & LOGISTICS
  // ============================================================

  // Ruta
  Term.SpanishTerm := 'Ruta';
  Term.EnglishTerms := TArray<string>.Create(
    'Route',
    'DeliveryRoute',
    'ShippingRoute',
    'DistributionRoute'
  );
  Term.Context := 'Delivery route for logistics planning. Defines customer delivery sequences and territories.';
  Term.DomainTags := TArray<string>.Create(
    'Logistics',
    'DeliveryManagement',
    'RouteOptimization',
    'Distribution'
  );
  FTerms.Add('Ruta', Term);

  // Rutas
  Term.SpanishTerm := 'Rutas';
  Term.EnglishTerms := TArray<string>.Create(
    'Routes',
    'DeliveryRoutes',
    'ShippingRoutes',
    'DistributionRoutes'
  );
  Term.Context := 'Plural form. Multiple delivery route configurations.';
  Term.DomainTags := TArray<string>.Create(
    'Logistics',
    'DeliveryManagement',
    'RouteOptimization',
    'Distribution'
  );
  FTerms.Add('Rutas', Term);

  // SEPA
  Term.SpanishTerm := 'SEPA';
  Term.EnglishTerms := TArray<string>.Create(
    'SEPA',
    'SingleEuroPaymentsArea',
    'EuropeanPayments',
    'SEPATransfer',
    'SEPADirectDebit'
  );
  Term.Context := 'Single Euro Payments Area standard for European bank transfers and direct debits. XML-based format replacing Spanish CSB norms.';
  Term.DomainTags := TArray<string>.Create(
    'BankingOperations',
    'PaymentProcessing',
    'EuropeanStandards',
    'SEPA'
  );
  FTerms.Add('SEPA', Term);

  // SERCLI
  Term.SpanishTerm := 'SERCLI';
  Term.EnglishTerms := TArray<string>.Create(
    'CustomerService',
    'ServiceOrders',
    'ClientService',
    'PendingService',
    'ServiceBacklog',
    'CustomerServiceQuantity'
  );
  Term.Context := 'Pending customer service quantity data class (Servicio Cliente). Tracks outstanding service commitments to customers. Incrementable (+/-) class.';
  Term.DomainTags := TArray<string>.Create(
    'ServiceManagement',
    'CustomerManagement',
    'OrderManagement',
    'ServiceTracking'
  );
  FTerms.Add('SERCLI', Term);

  // SERPRV
  Term.SpanishTerm := 'SERPRV';
  Term.EnglishTerms := TArray<string>.Create(
    'SupplierService',
    'VendorService',
    'PendingSupplierService',
    'ServiceFromSupplier',
    'SupplierServiceQuantity',
    'VendorServiceBacklog'
  );
  Term.Context := 'Pending supplier service quantity data class (Servicio Proveedor). Tracks outstanding service commitments from suppliers. Incrementable (+/-) class.';
  Term.DomainTags := TArray<string>.Create(
    'ServiceManagement',
    'SupplierManagement',
    'Procurement',
    'ServiceTracking'
  );
  FTerms.Add('SERPRV', Term);

  // SII
  Term.SpanishTerm := 'SII';
  Term.EnglishTerms := TArray<string>.Create(
    'ImmediateSupplyOfInformation',
    'ElectronicInvoicing',
    'TaxAuthorityCommunication',
    'SII',
    'OnlineTaxReporting'
  );
  Term.Context := 'Sistema de Información Inmediata - Real-time electronic invoice submission to Spanish Tax Authority (AEAT). Mandatory for large companies.';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'ElectronicInvoicing',
    'SpanishTax',
    'AEAT',
    'VAT'
  );
  FTerms.Add('SII', Term);

  // ============================================================
  // Category: DATA CLASSES (ClaseDato)
  // ============================================================

  // STOCK
  Term.SpanishTerm := 'STOCK';
  Term.EnglishTerms := TArray<string>.Create(
    'Stock',
    'Inventory',
    'StockLevel',
    'InventoryLevel',
    'OnHand',
    'Quantity'
  );
  Term.Context := 'Physical inventory quantity data class. Incrementable (+/-) class segmented by warehouse, size, color, and lot for multi-dimensional inventory tracking.';
  Term.DomainTags := TArray<string>.Create(
    'InventoryTracking',
    'StockControl',
    'WarehouseManagement',
    'QuantityManagement'
  );
  FTerms.Add('STOCK', Term);

  // STOCKMIN
  Term.SpanishTerm := 'STOCKMIN';
  Term.EnglishTerms := TArray<string>.Create(
    'MinimumStock',
    'ReorderPoint',
    'SafetyStock',
    'MinimumInventory',
    'StockMinimum',
    'MinLevel'
  );
  Term.Context := 'Minimum stock level threshold for reorder alerts. Triggers procurement when stock falls below this level. Non-incrementable (=) class.';
  Term.DomainTags := TArray<string>.Create(
    'InventoryPlanning',
    'StockControl',
    'ReorderManagement',
    'WarehouseManagement'
  );
  FTerms.Add('STOCKMIN', Term);

  // Seccion
  Term.SpanishTerm := 'Seccion';
  Term.EnglishTerms := TArray<string>.Create(
    'Section',
    'Phase',
    'ProjectPhase',
    'ProjectSection',
    'Division'
  );
  Term.Context := 'A section or phase within a project (variant spelling without accent). Same as Sección.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'ProjectPlanning',
    'ProjectTracking',
    'BusinessEntity'
  );
  FTerms.Add('Seccion', Term);

  // Secciones
  Term.SpanishTerm := 'Secciones';
  Term.EnglishTerms := TArray<string>.Create(
    'Sections',
    'Phases',
    'ProjectPhases',
    'ProjectSections',
    'Divisions'
  );
  Term.Context := 'Multiple sections or phases within projects. Used to organize project work into manageable divisions.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'ProjectPlanning',
    'ProjectTracking',
    'BusinessEntity'
  );
  FTerms.Add('Secciones', Term);

  // Sección
  Term.SpanishTerm := 'Sección';
  Term.EnglishTerms := TArray<string>.Create(
    'Section',
    'Phase',
    'ProjectPhase',
    'ProjectSection',
    'Division',
    'Subdivision'
  );
  Term.Context := 'A section or phase within a project. Projects are divided into sections for better organization and cost tracking. Each work report (parte) is assigned to a specific section.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'ProjectPlanning',
    'ProjectTracking',
    'BusinessEntity'
  );
  FTerms.Add('Sección', Term);

  // Segmento
  Term.SpanishTerm := 'Segmento';
  Term.EnglishTerms := TArray<string>.Create(
    'Segment',
    'Dimension',
    'InventoryDimension',
    'TrackingDimension',
    'Attribute',
    'SegmentationAxis'
  );
  Term.Context := 'Inventory segmentation dimension (Almacen, Talla, Color, Lote, etc.). Defines axes for multi-dimensional inventory tracking in event sourcing system.';
  Term.DomainTags := TArray<string>.Create(
    'Segmentation',
    'InventoryTracking',
    'DataModeling',
    'MultiDimensional'
  );
  FTerms.Add('Segmento', Term);

  // Segmentos
  Term.SpanishTerm := 'Segmentos';
  Term.EnglishTerms := TArray<string>.Create(
    'Segments',
    'Dimensions',
    'InventoryDimensions',
    'TrackingDimensions',
    'Attributes',
    'SegmentationAxes'
  );
  Term.Context := 'Multiple inventory segmentation dimensions. Combination of tracking attributes for multi-dimensional inventory control.';
  Term.DomainTags := TArray<string>.Create(
    'Segmentation',
    'InventoryTracking',
    'DataModeling',
    'MultiDimensional'
  );
  FTerms.Add('Segmentos', Term);

  // ============================================================
  // Category: CRM & MARKETING
  // ============================================================

  // Seguimiento
  Term.SpanishTerm := 'Seguimiento';
  Term.EnglishTerms := TArray<string>.Create(
    'Tracking',
    'FollowUp',
    'CRMActivity',
    'CustomerTracking'
  );
  Term.Context := 'Commercial tracking and CRM activity management. Customer interaction tracking, activities, and follow-ups.';
  Term.DomainTags := TArray<string>.Create(
    'CRM',
    'SalesManagement',
    'ActivityTracking',
    'CustomerEngagement'
  );
  FTerms.Add('Seguimiento', Term);

  // Serie
  Term.SpanishTerm := 'Serie';
  Term.EnglishTerms := TArray<string>.Create(
    'SerialNumber',
    'Serial',
    'SeriesNumber',
    'ItemSerial',
    'Series',
    'Sequence'
  );
  Term.Context := 'Serial number for individual item tracking. Used for high-value products requiring unit-level traceability.';
  Term.DomainTags := TArray<string>.Create(
    'SerialNumbers',
    'Traceability',
    'Inventory',
    'AssetTracking'
  );
  FTerms.Add('Serie', Term);

  // Series
  Term.SpanishTerm := 'Series';
  Term.EnglishTerms := TArray<string>.Create(
    'SerialNumbers',
    'Serials',
    'SeriesNumbers',
    'ItemSerials',
    'Series',
    'Sequences'
  );
  Term.Context := 'Plural form. Multiple serial numbers for tracking.';
  Term.DomainTags := TArray<string>.Create(
    'SerialNumbers',
    'Traceability',
    'Inventory',
    'AssetTracking'
  );
  FTerms.Add('Series', Term);

  // ============================================================
  // Category: SERVER & CLIENT COMMUNICATION
  // ============================================================

  // Servidor
  Term.SpanishTerm := 'Servidor';
  Term.EnglishTerms := TArray<string>.Create(
    'Server',
    'ProtectionServer',
    'ApplicationServer',
    'DatabaseServer',
    'RemoteServer'
  );
  Term.Context := 'Server component in client-server architecture. Includes protection server for licensing and application server for business logic.';
  Term.DomainTags := TArray<string>.Create(
    'ClientServer',
    'NetworkCommunication',
    'MultiTier',
    'Licensing'
  );
  FTerms.Add('Servidor', Term);

  // ServidorProteccion
  Term.SpanishTerm := 'ServidorProteccion';
  Term.EnglishTerms := TArray<string>.Create(
    'ProtectionServer',
    'LicenseServer',
    'AuthenticationServer',
    'SecurityServer'
  );
  Term.Context := 'Dedicated server for managing software licensing, copy protection, and user authentication in multi-user installations.';
  Term.DomainTags := TArray<string>.Create(
    'Licensing',
    'Security',
    'CopyProtection',
    'MultiUser'
  );
  FTerms.Add('ServidorProteccion', Term);

  // SinIniciar
  Term.SpanishTerm := 'SinIniciar';
  Term.EnglishTerms := TArray<string>.Create(
    'NotStarted',
    'Pending',
    'Waiting',
    'Queued'
  );
  Term.Context := 'Production phase configured but not yet started';
  Term.DomainTags := TArray<string>.Create(
    'ProcessControl',
    'PhaseManagement',
    'WorkflowStatus'
  );
  FTerms.Add('SinIniciar', Term);

  // SinLanzar
  Term.SpanishTerm := 'SinLanzar';
  Term.EnglishTerms := TArray<string>.Create(
    'NotReleased',
    'NotLaunched',
    'Held',
    'NotStarted'
  );
  Term.Context := 'Production phase not yet released to shop floor';
  Term.DomainTags := TArray<string>.Create(
    'ProcessControl',
    'PhaseManagement',
    'ShopFloorControl'
  );
  FTerms.Add('SinLanzar', Term);

  // Soportes
  Term.SpanishTerm := 'Soportes';
  Term.EnglishTerms := TArray<string>.Create(
    'Supports',
    'MediaSupports',
    'Structures',
    'BillboardStructures',
    'MediaFrameworks'
  );
  Term.Context := 'Physical support structures for advertising media including type, dimensions, materials, and installation specifications. Infrastructure asset category.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'Infrastructure',
    'AssetManagement',
    'PhysicalAssets'
  );
  FTerms.Add('Soportes', Term);

  // Stock
  Term.SpanishTerm := 'Stock';
  Term.EnglishTerms := TArray<string>.Create(
    'Stock',
    'Inventory',
    'StockLevel',
    'InventoryLevel'
  );
  Term.Context := 'Stock level or inventory quantity. Same as Existencia but using English loanword common in Spanish business.';
  Term.DomainTags := TArray<string>.Create(
    'Inventory',
    'StockControl',
    'WarehouseManagement',
    'InventoryManagement'
  );
  FTerms.Add('Stock', Term);

  // Subanunciantes
  Term.SpanishTerm := 'Subanunciantes';
  Term.EnglishTerms := TArray<string>.Create(
    'SubAdvertisers',
    'SubBrands',
    'ProductLines',
    'SubClients'
  );
  Term.Context := 'Sub-advertiser entities under main advertisers representing product lines or divisions within advertiser organizations. Enables hierarchical campaign management.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'CustomerManagement',
    'HierarchicalStructure',
    'CampaignManagement'
  );
  FTerms.Add('Subanunciantes', Term);

  // Subdiario
  Term.SpanishTerm := 'Subdiario';
  Term.EnglishTerms := TArray<string>.Create(
    'SubJournal',
    'SpecialJournal',
    'SubsidiaryJournal',
    'AuxiliaryJournal',
    'Journal Division'
  );
  Term.Context := 'Specialized journal for specific transaction types (e.g., sales, purchases). Entries are later transferred to the general journal.';
  Term.DomainTags := TArray<string>.Create(
    'GeneralLedger',
    'JournalEntries',
    'AccountingBooks'
  );
  FTerms.Add('Subdiario', Term);

  // Suborden
  Term.SpanishTerm := 'Suborden';
  Term.EnglishTerms := TArray<string>.Create(
    'Suborder',
    'ChildOrder',
    'DependentOrder',
    'LinkedOrder'
  );
  Term.Context := 'Suborder linked to parent order';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'ProcessControl',
    'Manufacturing'
  );
  FTerms.Add('Suborden', Term);

  // ============================================================
  // Category: SUBORDERS
  // ============================================================

  // Subordenes
  Term.SpanishTerm := 'Subordenes';
  Term.EnglishTerms := TArray<string>.Create(
    'Suborders',
    'ChildOrders',
    'DependentOrders',
    'LinkedOrders'
  );
  Term.Context := 'Suborders linked to parent production orders for complex multi-step manufacturing';
  Term.DomainTags := TArray<string>.Create(
    'OrderManagement',
    'ProcessControl',
    'Manufacturing',
    'ComplexManufacturing'
  );
  FTerms.Add('Subordenes', Term);

  // ============================================================
  // Category: DATABASE OPERATIONS
  // ============================================================

  // Tabla
  Term.SpanishTerm := 'Tabla';
  Term.EnglishTerms := TArray<string>.Create(
    'Table',
    'DatabaseTable',
    'DataTable',
    'Entity'
  );
  Term.Context := 'Database table component with custom wrappers for BDE, ElevateDB, and modern data access.';
  Term.DomainTags := TArray<string>.Create(
    'DatabaseAccess',
    'DataComponents',
    'TableManagement'
  );
  FTerms.Add('Tabla', Term);

  // Tablas
  Term.SpanishTerm := 'Tablas';
  Term.EnglishTerms := TArray<string>.Create(
    'Tables',
    'DatabaseTables',
    'DataTables'
  );
  Term.Context := 'Multiple database tables or table management system.';
  Term.DomainTags := TArray<string>.Create(
    'DatabaseAccess',
    'DataComponents',
    'TableManagement'
  );
  FTerms.Add('Tablas', Term);

  // Talla
  Term.SpanishTerm := 'Talla';
  Term.EnglishTerms := TArray<string>.Create(
    'Size',
    'ProductSize',
    'SizeVariant',
    'Sizing',
    'Sizing',
    'SizeDimension'
  );
  Term.Context := 'Product size variant dimension. Used with Color for fashion retail matrix management (color/size grids).';
  Term.DomainTags := TArray<string>.Create(
    'ProductVariants',
    'FashionRetail',
    'ProductAttributes',
    'VariantManagement'
  );
  FTerms.Add('Talla', Term);

  // Tallas
  Term.SpanishTerm := 'Tallas';
  Term.EnglishTerms := TArray<string>.Create(
    'Sizes',
    'ProductSizes',
    'SizeVariants',
    'Sizings'
  );
  Term.Context := 'Plural form. Size variant catalog.';
  Term.DomainTags := TArray<string>.Create(
    'ProductVariants',
    'FashionRetail',
    'ProductAttributes',
    'VariantManagement'
  );
  FTerms.Add('Tallas', Term);

  // Tarea
  Term.SpanishTerm := 'Tarea';
  Term.EnglishTerms := TArray<string>.Create(
    'Task',
    'Operation',
    'Activity',
    'WorkStep'
  );
  Term.Context := 'Individual manufacturing task or operation';
  Term.DomainTags := TArray<string>.Create(
    'ProcessPlanning',
    'TaskManagement',
    'Routing'
  );
  FTerms.Add('Tarea', Term);

  // ============================================================
  // Category: TASKS
  // ============================================================

  // Tareas
  Term.SpanishTerm := 'Tareas';
  Term.EnglishTerms := TArray<string>.Create(
    'Tasks',
    'Operations',
    'Activities',
    'WorkInstructions'
  );
  Term.Context := 'Manufacturing tasks defined within production routes and phases';
  Term.DomainTags := TArray<string>.Create(
    'ProcessPlanning',
    'TaskManagement',
    'Manufacturing',
    'Routing'
  );
  FTerms.Add('Tareas', Term);

  // ============================================================
  // Category: PRICING & SALES
  // ============================================================

  // Tarifa
  Term.SpanishTerm := 'Tarifa';
  Term.EnglishTerms := TArray<string>.Create(
    'Tariff',
    'PriceList',
    'PricingSchedule',
    'RateCard'
  );
  Term.Context := 'Price list or tariff with multi-dimensional pricing (customer, product, quantity). Sophisticated pricing system with 15+ components.';
  Term.DomainTags := TArray<string>.Create(
    'Pricing',
    'SalesManagement',
    'PriceManagement',
    'CustomerPricing'
  );
  FTerms.Add('Tarifa', Term);

  // Tarifas
  Term.SpanishTerm := 'Tarifas';
  Term.EnglishTerms := TArray<string>.Create(
    'Tariffs',
    'PriceLists',
    'PricingSchedules',
    'RateCards',
    'RateCards',
    'PriceTables'
  );
  Term.Context := 'Plural form. Multiple pricing schedules and price lists.';
  Term.DomainTags := TArray<string>.Create(
    'Pricing',
    'SalesManagement',
    'PriceManagement',
    'CustomerPricing'
  );
  FTerms.Add('Tarifas', Term);

  // Teléfono
  Term.SpanishTerm := 'Teléfono';
  Term.EnglishTerms := TArray<string>.Create(
    'Phone',
    'Telephone',
    'PhoneNumber',
    'Contact'
  );
  Term.Context := 'Telephone or phone number field. Contact phone information.';
  Term.DomainTags := TArray<string>.Create(
    'DataFields',
    'ContactInformation',
    'CommunicationData',
    'PhoneNumbers'
  );
  FTerms.Add('Teléfono', Term);

  // Terminar
  Term.SpanishTerm := 'Terminar';
  Term.EnglishTerms := TArray<string>.Create(
    'Finish',
    'End',
    'Complete',
    'Terminate'
  );
  Term.Context := 'Finish or terminate operation. Ends processes, closes dialogs, or completes operations.';
  Term.DomainTags := TArray<string>.Create(
    'ProcessManagement',
    'OperationCompletion',
    'ApplicationLifecycle'
  );
  FTerms.Add('Terminar', Term);

  // TipoTarea
  Term.SpanishTerm := 'TipoTarea';
  Term.EnglishTerms := TArray<string>.Create(
    'TaskType',
    'OperationType',
    'ActivityType',
    'WorkCategory'
  );
  Term.Context := 'Task type definition';
  Term.DomainTags := TArray<string>.Create(
    'ProcessPlanning',
    'TaskManagement'
  );
  FTerms.Add('TipoTarea', Term);

  // TiposAvion
  Term.SpanishTerm := 'TiposAvion';
  Term.EnglishTerms := TArray<string>.Create(
    'AircraftTypes',
    'AircraftModels',
    'AircraftCategories',
    'PlaneTypes'
  );
  Term.Context := 'Aircraft type classifications with IATA codes, capacities, and parking requirements. Used for service planning and pricing.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'FleetManagement',
    'MasterData',
    'Classification'
  );
  FTerms.Add('TiposAvion', Term);

  // TiposTarea
  Term.SpanishTerm := 'TiposTarea';
  Term.EnglishTerms := TArray<string>.Create(
    'TaskTypes',
    'OperationTypes',
    'ActivityTypes',
    'WorkCategories'
  );
  Term.Context := 'Task type definitions for categorizing manufacturing operations';
  Term.DomainTags := TArray<string>.Create(
    'ProcessPlanning',
    'TaskManagement',
    'Configuration'
  );
  FTerms.Add('TiposTarea', Term);

  // Totales
  Term.SpanishTerm := 'Totales';
  Term.EnglishTerms := TArray<string>.Create(
    'Totals',
    'Total',
    'Sum',
    'Aggregate'
  );
  Term.Context := 'Total quantities or costs aggregated';
  Term.DomainTags := TArray<string>.Create(
    'Metrics',
    'Reporting',
    'Analytics'
  );
  FTerms.Add('Totales', Term);

  // Trabajo
  Term.SpanishTerm := 'Trabajo';
  Term.EnglishTerms := TArray<string>.Create(
    'Work',
    'Labor',
    'LaborCost',
    'LaborWork',
    'Task',
    'WorkTask'
  );
  Term.Context := 'Work or labor performed on a project. Tracks worker time and labor costs separate from materials and expenses. Recorded in labor work reports (partes trabajo).';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'JobCosting',
    'LaborManagement',
    'TimeTracking'
  );
  FTerms.Add('Trabajo', Term);

  // Trabajos
  Term.SpanishTerm := 'Trabajos';
  Term.EnglishTerms := TArray<string>.Create(
    'Works',
    'Labor',
    'LaborCosts',
    'LaborWorks',
    'Tasks',
    'WorkTasks'
  );
  Term.Context := 'Multiple work items or labor entries. One of the three main cost categories in project management.';
  Term.DomainTags := TArray<string>.Create(
    'ProjectManagement',
    'JobCosting',
    'LaborManagement',
    'TimeTracking'
  );
  FTerms.Add('Trabajos', Term);

  // ============================================================
  // Category: AVIATION
  Term.SpanishTerm := 'Tripulantes';
  Term.EnglishTerms := TArray<string>.Create(
    'CrewMembers',
    'FlightCrew',
    'Crew',
    'Pilots',
    'CabinCrew'
  );
  Term.Context := 'Flight crew members including pilots and cabin crew. Tracked for accommodation and service coordination requirements.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'CrewManagement',
    'HumanResources',
    'FlightOperations'
  );
  FTerms.Add('Tripulantes', Term);

  // Unidades
  Term.SpanishTerm := 'Unidades';
  Term.EnglishTerms := TArray<string>.Create(
    'Units',
    'Quantity',
    'Amount',
    'Pieces'
  );
  Term.Context := 'Production quantity in units';
  Term.DomainTags := TArray<string>.Create(
    'ProductionTracking',
    'Metrics',
    'Quantity'
  );
  FTerms.Add('Unidades', Term);

  // ============================================================
  // Category: USER & SECURITY MANAGEMENT
  // ============================================================

  // Usuario
  Term.SpanishTerm := 'Usuario';
  Term.EnglishTerms := TArray<string>.Create(
    'User',
    'SystemUser',
    'ApplicationUser',
    'EndUser'
  );
  Term.Context := 'System user with credentials and permissions. Tracked for licensing, audit trails, and access control.';
  Term.DomainTags := TArray<string>.Create(
    'UserManagement',
    'Security',
    'AccessControl',
    'AuditTrail'
  );
  FTerms.Add('Usuario', Term);

  // Usuarios
  Term.SpanishTerm := 'Usuarios';
  Term.EnglishTerms := TArray<string>.Create(
    'Users',
    'SystemUsers',
    'UserAccounts'
  );
  Term.Context := 'Multiple system users or user management system.';
  Term.DomainTags := TArray<string>.Create(
    'UserManagement',
    'Security',
    'AccessControl'
  );
  FTerms.Add('Usuarios', Term);

  // Vaciar
  Term.SpanishTerm := 'Vaciar';
  Term.EnglishTerms := TArray<string>.Create(
    'Empty',
    'Clear',
    'Purge',
    'Delete',
    'Truncate',
    'RemoveAll'
  );
  Term.Context := 'Clear or empty table operation. Removes all records from DatArt table before recalculation, typically used in maintenance procedures.';
  Term.DomainTags := TArray<string>.Create(
    'DataMaintenance',
    'SystemMaintenance',
    'DatabaseOperations',
    'TableManagement'
  );
  FTerms.Add('Vaciar', Term);

  // ============================================================
  // Category: ADVERTISING/BILLBOARD MANAGEMENT (GAN)
  // ============================================================

  // Vallas
  Term.SpanishTerm := 'Vallas';
  Term.EnglishTerms := TArray<string>.Create(
    'Billboards',
    'OutdoorAdvertising',
    'AdvertisingBoards',
    'Signs',
    'OutOfHomeMedia'
  );
  Term.Context := 'Billboard/outdoor advertising inventory including location, size, orientation, visibility, and availability. Core asset in advertising operations management.';
  Term.DomainTags := TArray<string>.Create(
    'Advertising',
    'OutdoorMedia',
    'AssetManagement',
    'InventoryManagement'
  );
  FTerms.Add('Vallas', Term);

  // ValorFecha
  Term.SpanishTerm := 'ValorFecha';
  Term.EnglishTerms := TArray<string>.Create(
    'DateValue',
    'DateField',
    'TemporalValue',
    'DateData',
    'TimestampValue',
    'DateAttribute'
  );
  Term.Context := 'Date value field for temporal product data. Stores dates like expiration, manufacturing date, warranty dates in event sourcing system.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'DataStorage',
    'DateTracking',
    'TemporalData'
  );
  FTerms.Add('ValorFecha', Term);

  // ValorImagen
  Term.SpanishTerm := 'ValorImagen';
  Term.EnglishTerms := TArray<string>.Create(
    'ImageValue',
    'Picture',
    'Photo',
    'Graphic',
    'BinaryImage',
    'ProductImage'
  );
  Term.Context := 'Image field for storing product images and graphics in event sourcing system. Binary storage for visual product data.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'DataStorage',
    'ImageManagement',
    'MultimediaData'
  );
  FTerms.Add('ValorImagen', Term);

  // ValorLogico
  Term.SpanishTerm := 'ValorLogico';
  Term.EnglishTerms := TArray<string>.Create(
    'BooleanValue',
    'LogicalValue',
    'TrueFalseValue',
    'FlagValue',
    'BinaryValue',
    'YesNoValue'
  );
  Term.Context := 'Boolean/logical value field for true/false product attributes in event sourcing system. Stores flags and binary states.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'DataStorage',
    'BooleanData',
    'FlagManagement'
  );
  FTerms.Add('ValorLogico', Term);

  // ValorMemo
  Term.SpanishTerm := 'ValorMemo';
  Term.EnglishTerms := TArray<string>.Create(
    'MemoValue',
    'LongText',
    'TextArea',
    'Notes',
    'Description',
    'LargeText'
  );
  Term.Context := 'Memo field for long text product data. Stores detailed descriptions, notes, and large textual content in event sourcing system.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'DataStorage',
    'TextData',
    'Documentation'
  );
  FTerms.Add('ValorMemo', Term);

  // ValorNum
  Term.SpanishTerm := 'ValorNum';
  Term.EnglishTerms := TArray<string>.Create(
    'NumericValue',
    'NumberValue',
    'QuantityValue',
    'Amount',
    'Value',
    'NumValue'
  );
  Term.Context := 'Numeric value field in events and data tables. Stores quantities, prices, costs, dimensions, and other numeric product data.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'DataStorage',
    'NumericData',
    'ValueTracking'
  );
  FTerms.Add('ValorNum', Term);

  // ValorTexto
  Term.SpanishTerm := 'ValorTexto';
  Term.EnglishTerms := TArray<string>.Create(
    'TextValue',
    'StringValue',
    'TextData',
    'StringData',
    'AlphanumericValue',
    'CharacterValue'
  );
  Term.Context := 'Text value field for string-based product data in event sourcing system. Stores textual attributes and descriptions.';
  Term.DomainTags := TArray<string>.Create(
    'EventSourcing',
    'DataStorage',
    'TextData',
    'AttributeStorage'
  );
  FTerms.Add('ValorTexto', Term);

  // Variante
  Term.SpanishTerm := 'Variante';
  Term.EnglishTerms := TArray<string>.Create(
    'Variant',
    'ProductVariant',
    'ItemVariant',
    'SKUVariant'
  );
  Term.Context := 'Product variant combining attributes like color and size. Represents a specific SKU variation.';
  Term.DomainTags := TArray<string>.Create(
    'ProductVariants',
    'ProductManagement',
    'VariantManagement',
    'SKUManagement'
  );
  FTerms.Add('Variante', Term);

  // Vencimiento
  Term.SpanishTerm := 'Vencimiento';
  Term.EnglishTerms := TArray<string>.Create(
    'DueDate',
    'Maturity',
    'ExpirationDate',
    'PaymentDate',
    'MaturityDate',
    'PaymentDue'
  );
  Term.Context := 'Due date or maturity date for payments, bills, or invoices. Critical for cash flow management and collections.';
  Term.DomainTags := TArray<string>.Create(
    'PaymentProcessing',
    'AccountsReceivable',
    'AccountsPayable',
    'CashManagement'
  );
  FTerms.Add('Vencimiento', Term);

  // Vencimientos
  Term.SpanishTerm := 'Vencimientos';
  Term.EnglishTerms := TArray<string>.Create(
    'DueDates',
    'Maturities',
    'ExpirationDates',
    'PaymentDates',
    'MaturityDates',
    'PaymentsDue'
  );
  Term.Context := 'Multiple due dates or maturity schedule. Used in cash flow forecasting and payment planning.';
  Term.DomainTags := TArray<string>.Create(
    'PaymentProcessing',
    'CashManagement',
    'Planning',
    'PaymentTerms'
  );
  FTerms.Add('Vencimientos', Term);

  // ============================================================
  // Category: SPECIALIZED MODULES
  // ============================================================

  // VeriFactu
  Term.SpanishTerm := 'VeriFactu';
  Term.EnglishTerms := TArray<string>.Create(
    'VeriFactu',
    'InvoiceValidation',
    'SpanishTaxCompliance',
    'InvoiceVerification'
  );
  Term.Context := 'VeriFactu - Spanish invoice validation and anti-fraud system. Native Delphi implementation for AEAT compliance with direct SOAP integration.';
  Term.DomainTags := TArray<string>.Create(
    'TaxCompliance',
    'SpanishRegulation',
    'InvoiceValidation',
    'AEATCompliance'
  );
  FTerms.Add('VeriFactu', Term);

  // ============================================================
  // CLIENTES MODULE GLOSSARY
  // ============================================================
  // ============================================================
  // Category: AVIATION
  Term.SpanishTerm := 'Vuelos';
  Term.EnglishTerms := TArray<string>.Create(
    'Flights',
    'FlightOperations',
    'FlightMovements',
    'AircraftOperations',
    'AviationOperations'
  );
  Term.Context := 'Flight operations management including arrivals, departures, slots, and billing. Core entity for FBO daily operations tracking all flight movements.';
  Term.DomainTags := TArray<string>.Create(
    'Aviation',
    'FlightOperations',
    'FBOoperations',
    'AirportOperations'
  );
  FTerms.Add('Vuelos', Term);

  // ZonaComercial
  Term.SpanishTerm := 'ZonaComercial';
  Term.EnglishTerms := TArray<string>.Create(
    'SalesZone',
    'SalesTerritory',
    'CommercialZone',
    'TerritoryManagement'
  );
  Term.Context := 'Sales zone or territory for customer segmentation and agent assignment. Geographic or strategic customer grouping.';
  Term.DomainTags := TArray<string>.Create(
    'SalesManagement',
    'TerritoryManagement',
    'CustomerSegmentation',
    'SalesOrganization'
  );
  FTerms.Add('ZonaComercial', Term);

  // ZonasComercial
  Term.SpanishTerm := 'ZonasComercial';
  Term.EnglishTerms := TArray<string>.Create(
    'SalesZones',
    'SalesTerritories',
    'CommercialZones',
    'TerritoryStructure'
  );
  Term.Context := 'Plural form. Sales territory structure and zones.';
  Term.DomainTags := TArray<string>.Create(
    'SalesManagement',
    'TerritoryManagement',
    'CustomerSegmentation',
    'SalesOrganization'
  );
  FTerms.Add('ZonasComercial', Term);

  

  // ============================================================
  // ABBREVIATIONS (ALL MODULES)
  // ============================================================


  FAbbreviations.Add('AEAT', 'Agencia Estatal de Administración Tributaria');
  FAbbreviations.Add('Age', 'Agente');
  FAbbreviations.Add('Alb', 'Albarán');
  FAbbreviations.Add('Alm', 'Almacén');
  FAbbreviations.Add('Alq', 'Alquiler');
  FAbbreviations.Add('Anunc', 'Anunciante');
  FAbbreviations.Add('Apt', 'Aeropuerto');
  FAbbreviations.Add('Art', 'Artículo');
  FAbbreviations.Add('Arti', 'Artículo');
  FAbbreviations.Add('CSB', 'Consejo Superior Bancario');
  FAbbreviations.Add('Ccpt', 'Concepto');
  FAbbreviations.Add('Circuv', 'Circuito Vallas');
  FAbbreviations.Add('Cli', 'Cliente');
  FAbbreviations.Add('Clie', 'Cliente');
  FAbbreviations.Add('Comp', 'Compañía');
  FAbbreviations.Add('Contr', 'Contrato');
  FAbbreviations.Add('Contrep', 'Contratación Empresa');
  FAbbreviations.Add('Coord', 'Coordinación');
  FAbbreviations.Add('Cos', 'Costes');
  FAbbreviations.Add('Cta', 'Cuenta');
  FAbbreviations.Add('DatArt', 'DatosArticulos');
  FAbbreviations.Add('Dest', 'Destruidas');
  FAbbreviations.Add('DiaPr', 'Diario de Producción');
  FAbbreviations.Add('Emp', 'Empresa');
  FAbbreviations.Add('Empl', 'Emplazamiento');
  FAbbreviations.Add('EvtArt', 'EventosArticulos');
  FAbbreviations.Add('Fac', 'Factura');
  FAbbreviations.Add('Fact', 'Factura');
  FAbbreviations.Add('Fam', 'Familia');
  FAbbreviations.Add('G2K', 'Gestion2000');
  FAbbreviations.Add('GAS', 'Gastos');
  FAbbreviations.Add('Hosp', 'Hospedaje');
  FAbbreviations.Add('IRPF', 'Impuesto sobre la Renta de las Personas Físicas');
  FAbbreviations.Add('IVA', 'Impuesto sobre el Valor Añadido');
  FAbbreviations.Add('Inv', 'Invertido');
  FAbbreviations.Add('Lin', 'Línea');
  FAbbreviations.Add('Lins', 'Líneas');
  FAbbreviations.Add('MATE', 'Materiales');
  FAbbreviations.Add('Maq', 'Máquinas');
  FAbbreviations.Add('Mat', 'Materiales');
  FAbbreviations.Add('Max', 'Máximas');
  FAbbreviations.Add('Min', 'Mínimas');
  FAbbreviations.Add('Num', 'Numero');
  FAbbreviations.Add('NumSer', 'Número de Serie');
  FAbbreviations.Add('OF', 'Orden de Fabricación');
  FAbbreviations.Add('Obj', 'Objetivo');
  FAbbreviations.Add('Op', 'Operario');
  FAbbreviations.Add('Ord', 'Orden');
  FAbbreviations.Add('PEDCLI', 'PedidoCliente');
  FAbbreviations.Add('PEDPRV', 'PedidoProveedor');
  FAbbreviations.Add('PGC', 'Plan General Contable');
  FAbbreviations.Add('PRY', 'Proyectos');
  FAbbreviations.Add('PVP', 'PrecioVentaPublico');
  FAbbreviations.Add('Ped', 'Pedido');
  FAbbreviations.Add('Peds', 'Pedidos');
  FAbbreviations.Add('Planif', 'Planificación');
  FAbbreviations.Add('Pr', 'Producción');
  FAbbreviations.Add('Pres', 'Presupuesto');
  FAbbreviations.Add('Presup', 'Presupuesto');
  FAbbreviations.Add('Prev', 'Previsto');
  FAbbreviations.Add('Prod', 'Producción');
  FAbbreviations.Add('Produ', 'Producción');
  FAbbreviations.Add('Prov', 'Proveedor');
  FAbbreviations.Add('Pry', 'Proyectos');
  FAbbreviations.Add('Real', 'Realizado');
  FAbbreviations.Add('Ref', 'Referencia');
  FAbbreviations.Add('SEPA', 'Single Euro Payments Area');
  FAbbreviations.Add('SERCLI', 'ServicioCliente');
  FAbbreviations.Add('SERPRV', 'ServicioProveedor');
  FAbbreviations.Add('SII', 'Sistema de Información Inmediata');
  FAbbreviations.Add('Srv', 'Servidor');
  FAbbreviations.Add('TRAB', 'Trabajos');
  FAbbreviations.Add('Trip', 'Tripulante');
  FAbbreviations.Add('Usr', 'Usuario');
  FAbbreviations.Add('Ven', 'Venta');
  FAbbreviations.Add('acd', 'ArticuloClaseDato');
end;

function TGlossaryEnricher.DetectSpanishTerms(const Code: string): TArray<string>;
var
  Term: TPair<string, TGlossaryTerm>;
  DetectedTerms: TList<string>;
  Pattern: string;
begin
  DetectedTerms := TList<string>.Create;
  try
    for Term in FTerms do
    begin
      // Word boundary regex pattern: \bFichar\w*\b
      // Matches: Fichar, FicharEstaFase, Fichaje, etc.
      Pattern := '\b' + Term.Key + '\w*\b';

      if TRegEx.IsMatch(Code, Pattern, [roIgnoreCase]) then
      begin
        if not DetectedTerms.Contains(Term.Key) then
          DetectedTerms.Add(Term.Key);
      end;
    end;

    Result := DetectedTerms.ToArray;
  finally
    DetectedTerms.Free;
  end;
end;

function TGlossaryEnricher.BuildEnrichmentMetadata(const Terms: TArray<string>): string;
var
  Translations: TStringList;
  Domains: TStringList;
  Term: string;
  GlossaryTerm: TGlossaryTerm;
  EnglishTerm: string;
  DomainTag: string;
begin
  if Length(Terms) = 0 then
  begin
    Result := '';  // No enrichment needed
    Exit;
  end;

  Translations := TStringList.Create;
  Domains := TStringList.Create;
  try
    Translations.Duplicates := dupIgnore;
    Translations.Sorted := True;
    Domains.Duplicates := dupIgnore;
    Domains.Sorted := True;

    // Collect translations and domains
    for Term in Terms do
    begin
      if FTerms.TryGetValue(Term, GlossaryTerm) then
      begin
        // Add translations: "Fichar=TimeTracking, Fichar=ClockIn"
        for EnglishTerm in GlossaryTerm.EnglishTerms do
          Translations.Add(Format('%s=%s', [Term, EnglishTerm]));

        // Add domain tags
        for DomainTag in GlossaryTerm.DomainTags do
          Domains.Add(DomainTag);
      end;
    end;

    // Build metadata block (Pascal comment format)
    Result := #13#10 + '{AI-Search-Metadata:' + #13#10;

    if Translations.Count > 0 then
      Result := Result + '  Translations: ' + Translations.CommaText + #13#10;

    if Domains.Count > 0 then
      Result := Result + '  Domains: ' + Domains.CommaText + #13#10;

    Result := Result + '}' + #13#10;
  finally
    Translations.Free;
    Domains.Free;
  end;
end;

function TGlossaryEnricher.EnrichChunk(const OriginalCode, ChunkName, ChunkType: string): string;
var
  DetectedTerms: TArray<string>;
  Metadata: string;
begin
  // Detect Spanish terms in the code
  DetectedTerms := DetectSpanishTerms(OriginalCode);

  if Length(DetectedTerms) = 0 then
  begin
    // No Spanish terms found - return original code unchanged
    Result := OriginalCode;
    Exit;
  end;

  // Build metadata block
  Metadata := BuildEnrichmentMetadata(DetectedTerms);

  // Append metadata to original code
  Result := OriginalCode + Metadata;
end;

function TGlossaryEnricher.ExtractDomainTags(const Code: string): string;
var
  DetectedTerms: TArray<string>;
  Domains: TStringList;
  Term: string;
  GlossaryTerm: TGlossaryTerm;
  DomainTag: string;
begin
  DetectedTerms := DetectSpanishTerms(Code);

  Domains := TStringList.Create;
  try
    Domains.Duplicates := dupIgnore;
    Domains.Sorted := True;

    for Term in DetectedTerms do
    begin
      if FTerms.TryGetValue(Term, GlossaryTerm) then
      begin
        for DomainTag in GlossaryTerm.DomainTags do
          Domains.Add(DomainTag);
      end;
    end;

    // Return CSV: "TimeTracking,ShopFloorControl,LaborManagement"
    Result := Domains.CommaText;
  finally
    Domains.Free;
  end;
end;

function TGlossaryEnricher.GetDetectedTerms(const Code: string): string;
var
  DetectedTerms: TArray<string>;
  JSONArray: TJSONArray;
  Term: string;
begin
  DetectedTerms := DetectSpanishTerms(Code);

  JSONArray := TJSONArray.Create;
  try
    for Term in DetectedTerms do
      JSONArray.Add(Term);

    // Return JSON: ["Fichar", "Operario"]
    Result := JSONArray.ToJSON;
  finally
    JSONArray.Free;
  end;
end;

function TGlossaryEnricher.ExpandAbbreviation(const Abbrev: string): string;
begin
  if not FAbbreviations.TryGetValue(Abbrev, Result) then
    Result := Abbrev;  // Return original if not found
end;

function TGlossaryEnricher.TermCount: Integer;
begin
  Result := FTerms.Count;
end;

end.
