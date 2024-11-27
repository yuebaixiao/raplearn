@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight Travel (Data Model)'
define root view entity z437A_R_TRAVEL
  as select from z2463_dtravel
  association to /DMO/I_Customer as _cus on $projection.CustomerId = _cus.CustomerID
  //composition of target_data_source_name as _association_name
  {
    key agency_id   as AgencyId,
    key travel_id   as TravelId,
        description as Description,
        customer_id as CustomerId,
        
        begin_date  as BeginDate,
        end_date    as EndDate,
        status      as Status,
        @Semantics.systemDateTime.lastChangedAt: true
        changed_at  as ChangedAt,
        @Semantics.user.lastChangedBy: true
        changed_by  as ChangedBy,
        _cus,
        _cus.FirstName as cname
  }
