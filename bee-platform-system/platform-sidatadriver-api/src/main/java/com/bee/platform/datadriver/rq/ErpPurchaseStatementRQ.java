package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @Classname ErpPurchaseStatementRQ
 * @Description 采购销售单请求信息
 * @Date 2019/5/30 20:34
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购销售单请求信息")
public class ErpPurchaseStatementRQ implements Serializable {
    private static final long serialVersionUID = -8881364010226616324L;

    @ApiModelProperty("采购公司id")
    private Integer companyId;

    @ApiModelProperty("采购订单号")
    private String contractNo;

    @ApiModelProperty("供应商名称")
    private String supplyName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("状态")
    private Integer state;

    @ApiModelProperty("结算单号")
    private String code;

    @ApiModelProperty("结算开始日期")
    private String statementStartTime;

    @ApiModelProperty("结算结束日期")
    private String statementEndTime;

    @ApiModelProperty("企业id列表")
    private List<Integer> enterpriseIdList;
}
