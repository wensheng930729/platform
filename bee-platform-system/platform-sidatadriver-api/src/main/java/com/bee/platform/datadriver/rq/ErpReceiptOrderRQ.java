package com.bee.platform.datadriver.rq;


import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 销售收款单主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("销售收款请求参数")
public class ErpReceiptOrderRQ  implements Serializable{

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    /**
     * 收款单编号
     */
    @ApiModelProperty("收款单编号")
    @NotEmpty(message = "收款单号不能为空")
    private String code;
    /**
     * 收款公司id
     */
    @ApiModelProperty("收款公司id")
    @NotNull(message = "收款公司id不能为空")
    private Integer companyId;

    /**
     * 收款日期
     */
    @ApiModelProperty("收款日期")
    @NotNull(message = "收款日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiptTime;
    /**
     * 客户id
     */
    @ApiModelProperty("客户id")
    @NotNull(message = "客户id不能为空")
    private Integer customerId;

    /**
     * 销售订单id
     */
    @ApiModelProperty("销售订单id")
    @NotNull(message = "销售订单id不能为空")
    private Integer saleOrderId;

    /**
     * 支付方式，从码表取值
     */
    @ApiModelProperty("支付方式，从码表取值")
    @NotNull(message = "支付方式不能为空")
    private String payMethod;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    @Length(max = 200,message = "备注信息不超过200字")
    private String remark;
    /**
//     * 收款金额
//     */
//    @ApiModelProperty("收款金额")
//    @NotNull(message = "收款金额不能为空")
//    @Min(value = 0,message = "收款金额不能小于0")
//    private BigDecimal amount;


    @ApiModelProperty("详情集合")
    @Size(min = 1,message = "详情集合不能为空")
    List<ErpReceiptOrderDetailRQ> detailRQList;



}
