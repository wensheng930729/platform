package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @Classname SaleStatementRQ
 * @Description 销售结算请求信息
 * @Date 2019/5/31 14:47
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售结算请求信息")
public class SaleStatementRQ implements Serializable {
    private static final long serialVersionUID = -7635511018307248180L;

    @ApiModelProperty("销售结算单id")
    private Integer id;

    @ApiModelProperty("结算单号")
    @NotEmpty(message = "结算单号不能为空")
    private String code;

    @ApiModelProperty("销售订单号")
    @NotEmpty(message = "销售订单号不能为空")
    private String saleOrder;

    @ApiModelProperty("销售订单id")
    @NotNull(message = "销售订单id不能为空")
    private Integer saleOrderId;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;

    @ApiModelProperty("附件url")
    private String url;

    @ApiModelProperty("销售结算明细请求信息")
    private List<ErpSaleStatementDetailRQ> saleStatementDetailList;

}
