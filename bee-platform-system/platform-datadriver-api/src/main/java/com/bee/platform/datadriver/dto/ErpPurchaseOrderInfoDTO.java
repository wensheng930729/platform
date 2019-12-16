package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName ErpPurchaseOrderInfoDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/29$ 16:26$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购订单列表信息")
@JsonInclude(JsonInclude.Include.ALWAYS)
public class ErpPurchaseOrderInfoDTO implements Serializable {
    private static final long serialVersionUID = -190786276268321745L;

    @ApiModelProperty("id")
    protected Integer id;

    @ApiModelProperty("明细表id")
    protected Integer subId;

    @ApiModelProperty("合同编号")
    private String contractNo;

    @ApiModelProperty("合同签订日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractTime;

    @ApiModelProperty("数量")
    private Integer num;

    @ApiModelProperty("供应商id")
    private Integer supplier;

    @ApiModelProperty("供应商名称")
    private String supplyName;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("产品")
    private String productName;

    @ApiModelProperty("执行状态")
    private String state;

    @ApiModelProperty("总金额")
    private BigDecimal amount;

    @ApiModelProperty("收货状态(0未收货 1部分收货 2全部收货)")
    private Integer receiveState;

    @ApiModelProperty("结算状态(0未结算 1已结算)")
    private Integer accountState;

    @ApiModelProperty("发票状态(0未开票 1已开票)")
    private Integer invoiceState;

    @ApiModelProperty("备注")
    private String remark;
}
