package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @Classname ErpSaleStatementSelectRQ
 * @Description 销售结算查询列表请求信息
 * @Date 2019/6/1 10:37
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售结算查询列表请求信息")
public class ErpSaleStatementSelectRQ implements Serializable {
    private static final long serialVersionUID = 8713871324624535425L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("销售订单号")
    private String saleOrder;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("状态")
    private Integer state;

    @ApiModelProperty("结算单号")
    private String code;

    @ApiModelProperty("结算日期开始时间")
    private String recordStartTime;

    @ApiModelProperty("结算日期结束时间")
    private String recordEndTime;

    @ApiModelProperty("企业id列表")
    private List<Integer> enterpriseIdList;
}
