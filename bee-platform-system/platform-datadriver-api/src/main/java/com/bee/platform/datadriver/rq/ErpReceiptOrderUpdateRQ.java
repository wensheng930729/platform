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
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

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
@ApiModel("编辑销售收款主表请求参数")
public class ErpReceiptOrderUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
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
     * 收款公司名称
     */
    @ApiModelProperty("收款公司名称")
//    @NotEmpty(message = "收款公司不能为空")
    private String companyName;
    /**
     * 收款日期
     */
    @ApiModelProperty("收款日期")
    @NotNull(message = "收款日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiptTime;
    /**
     * 客户公司id
     */
    @ApiModelProperty("客户公司id")
    @NotNull(message = "客户公司id不能为空")
    private Integer customerId;
    /**
     * 客户公司名称
     */
    @ApiModelProperty("客户公司名称")
//    @NotNull(message = "客户公司不能为空")
    private String customerName;
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
     * 收款金额
     */
    @ApiModelProperty("收款金额")
    @NotNull(message = "收款金额不能为空")
    @Min(value = 0,message = "收款金额不能小于0")
    private BigDecimal amount;
    /**
     * 附件名
     */
    @ApiModelProperty("附件名")
    private String fileName;
    /**
     * 附件url
     */
    @ApiModelProperty("附件url")
    private String fileUrl;

    @ApiModelProperty("详单合计金额")
    @NotNull(message = "详单合计金额不能为空")
    @Min(value = 0,message = "详情单合计金额不能小于0")
    private BigDecimal sum;


}
