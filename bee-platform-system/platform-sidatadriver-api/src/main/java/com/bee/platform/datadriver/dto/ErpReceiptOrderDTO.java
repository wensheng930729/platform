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
@ApiModel("销售收款返回信息")
@JsonInclude
public class ErpReceiptOrderDTO implements Serializable{

    private static final long serialVersionUID = 1L;
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 收款单编号
     */
    @ApiModelProperty("收款单编号")
    private String code;
    /**
     * 收款公司id
     */
    @ApiModelProperty("收款公司id")
    private Integer companyId;
    /**
     * 收款公司名称
     */
    @ApiModelProperty("收款公司名称")
    private String companyName;
    /**
     * 收款日期
     */
    @ApiModelProperty("收款日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiptTime;
    /**
     * 客户公司id
     */
    @ApiModelProperty("客户公司id")
    private Integer customerId;
    /**
     * 客户公司名称
     */
    @ApiModelProperty("客户公司名称")
    private String customerName;
    /**
     * 销售订单id
     */
    @ApiModelProperty("销售订单id")
    private Integer saleOrderId;

    @ApiModelProperty("销售订单编号")
    private String saleCode;

    /**
     * 支付方式，从码表取值
     */
    @ApiModelProperty("支付方式，从码表取值")
    private String payMethod;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String remark;
    /**
     * 收款金额
     */
    @ApiModelProperty("收款金额")
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

    @ApiModelProperty("收款状态（0已保存，1已收款）")
    private Integer state;

    @ApiModelProperty("销售收款明细返回信息")
    List<ErpReceiptOrderDetailDTO> detailDTOList;



}
