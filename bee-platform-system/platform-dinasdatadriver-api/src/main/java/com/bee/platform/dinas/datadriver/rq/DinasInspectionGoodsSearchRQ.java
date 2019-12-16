package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName DinasInspectionGoodsSaveRQ
 * @Description 功能描述
 * @Date 2019/8/13 17:08
 **/
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("砂石验货磅单条件搜索请求参数")
public class DinasInspectionGoodsSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("采购合同编号")
    private String purchaseOrderCode;


    @ApiModelProperty("供应商名称")
    private String supplierName;

    @ApiModelProperty("销售合同编号")
    private String saleOrderCode;


    @ApiModelProperty("订货商名称")
    private String buyerName;


    @ApiModelProperty("产品名称")
    private String productName;


    @ApiModelProperty("产品规格名称")
    private String productSpecName;


    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;

}
