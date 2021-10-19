@AbapCatalog.sqlViewName: 'ZBD400_CDS2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'BD-400: Sales order items'
define view ZBD400_SALES_ORDER_ITEM2_00 
as select from vbak
inner join vbap
    on vbap.vbeln = vbak.vbeln
left outer join makt
    on makt.matnr = vbap.matnr 
{
    key vbak.vbeln,
    key vbap.posnr,
    key makt.spras,
    vbak.auart,
    vbap.pstyv,
    vbap.matnr,
    vbap.aedat,
    makt.maktx
}
