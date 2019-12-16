package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName ErpPayOrderSaveRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/31$ 9:14$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("付款单保存请求信息")
public class ErpPayOrderSaveRQ implements Serializable {

    private static final long serialVersionUID = 2993874340695128670L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("编号")
    private String code;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("金额")
    private BigDecimal amount;

    @ApiModelProperty("付款日期")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    private Date payDate;

    @ApiModelProperty("采购单id")
    private Integer purchaseOrder;

    @ApiModelProperty("采购单编号")
    @NotEmpty(message = "采购单编号不能为空")
    private String purchaseOrderNo;

    @ApiModelProperty("供应商id")
    private Integer supplier;

    @ApiModelProperty("供应商名称")
    @NotEmpty(message = "供应商名称不能为空")
    private String supplyName;

    @ApiModelProperty("备注")
    private String remark;

    @ApiModelProperty("支付方式，从码表取值")
    @NotEmpty(message = "支付方式不能为空")
    private String payMethod;

    @ApiModelProperty("附件地址")
    private String url;

}
