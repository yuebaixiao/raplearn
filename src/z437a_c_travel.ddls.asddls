@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'z437A_C_TRAVEL'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

@Search.searchable: true
define root view entity z437A_C_TRAVEL
  provider contract transactional_query
  as projection on z437A_R_TRAVEL
{
  key uuid,
      @Search.defaultSearchElement: true
      AgencyId,
      TravelId,
      Description,
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition:
      [ { entity:
            { name: '/DMO/I_Customer_StdVH',
              element: 'CustomerID'
            },
            additionalBinding: [{ localElement: 'Description', element: 'FirstName', usage: #FILTER }],
            qualifier: '11', label: 'help1'
        },
        { entity:
           { name: '/DMO/I_Customer',
             element: 'CustomerID'
           },
           additionalBinding: [{ localElement: 'cname', element: 'FirstName', usage: #RESULT }],
           qualifier: '2', label: 'help2'
        }
      ]
      CustomerId,
      cname,
      BeginDate,
      EndDate,
      Status,
      ChangedAt,
      ChangedBy,
      LocChangedAt
}
