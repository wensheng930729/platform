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
 * @author dell
 * @version 1.0.0
 * @ClassName ErpReceiptSearchDTO
 * @Description 功能描述
 * @Date 2019/5/29 21:44
 **/


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("销售收款搜索返回")
@JsonInclude
public class ErpReceiptSearchDTO implements Serializable {
    private static final long serialVersionUID = 1L;
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 收款单编号
     */
    @ApiModelProperty("收款单编号")
    private String code;

    /**
     * 收款日期
     */
    @ApiModelProperty("收款日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiptTime;

    /**
     * 客户公司名称
     */
    @ApiModelProperty("客户公司名称")
    private String customerName;
    /**
     * 收款金额
     */
    @ApiModelProperty("收款金额")
    private BigDecimal amount;

    @ApiModelProperty("收款状态（0已保存，1已收款）")
    private Integer state;


}

