package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 采购结算表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */


@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("销售结算列表条件搜索请求参数")
public class DinasSaleSettlementSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("销售合同编号")
    private String saleOrderCode;

    @ApiModelProperty("订货商名称")
    private String buyerName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("结算状态（0未结算 1已结算）")
    private Integer status;

    @ApiModelProperty("验货日期开始时间")
    private String inspectionStartTime;

    @ApiModelProperty("验货日期结束时间")
    private String inspectionEndTime;
    
    @ApiModelProperty("结算日期开始时间")
    private String settlementStartTime;

    @ApiModelProperty("结算日期结束时间")
    private String settlementEndTime;


}
