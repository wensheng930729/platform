package com.bee.platform.dinas.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

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
@ApiModel("砂石验货磅单返回信息")
public class DinasInspectionGoodsDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("采购合同id")
    private Integer purchaseOrderId;

    @ApiModelProperty("采购合同编号")
    private String purchaseOrderCode;

    @ApiModelProperty("销售合同id")
    private Integer saleOrderId;

    @ApiModelProperty("销售合同编号")
    private String saleOrderCode;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("数量")
    private BigDecimal num;

    @ApiModelProperty("附件地址")
    private String url;

//    @ApiModelProperty("附件地址")
//    private List<String> urlList;

    @ApiModelProperty("验货日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date inspectionDate;


}
