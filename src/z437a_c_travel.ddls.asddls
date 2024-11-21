@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'z437A_C_TRAVEL'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

@Search.searchable: true
define root view entity z437A_C_TRAVEL
  provider contract transactional_query
  as projection on z437A_R_TRAVEL
{
      @Search.defaultSearchElement: true
  key AgencyId,
  key TravelId,
      Description,
      @Search.defaultSearchElement: true
      CustomerId,
      BeginDate,
      EndDate,
      Status,
      ChangedAt,
      ChangedBy
}
