@AbapCatalog.sqlViewName: 'ZBD400_CDS1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'BD-400: Sales order items'
define view zbd400_sales_order_item
  with parameters
    p_unit : abap.cuky( 5 ),
    p_date : abap.dats
  as select from vbap
{
  vbeln,
  posnr,
  matnr,
  netwr,
  waerk,
  currency_conversion(amount=>netwr,
                      source_currency=>waerk,
                      target_currency=>:p_unit,
                      exchange_rate_date=>:p_date,
                      exchange_rate_type=>'M'         //optional, 'M' is the default
//There are more optional parameters.
                     )                             as netwr2,
  :p_unit                                          as waerk2
}
