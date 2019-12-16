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
 * @ClassName ErpRepoReceiptDetailRawInDTO
 * @Description 功能描述
 * @Date 2019/6/5 11:43
 **/

@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("原料入库根据id查询详情入库明细返回信息")
@JsonInclude
public class ErpRepoReceiptDetailRawInDTO implements Serializable {

    private static final long serialVersionUID = 1L;
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 关联的仓库单据id
     */
    @ApiModelProperty("关联的原料入库单据id")
    private Integer receiptId;
    /**
     *
     */
    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;
    /**
     * 提单号
     */
    @ApiModelProperty("提单号")
    private String voucherNo;
    /**
     * 湿重
     */
    @ApiModelProperty("湿重")
    private BigDecimal wetWeight;
    /**
     * 单位
     */
    @ApiModelProperty("单位")
    private String unit;
    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
    private Integer repositoryId;

    /**
     * 仓库id
     */
    @ApiModelProperty("仓库名称")
    private String repositoryName;
    /**
     * 化验单id
     */
    @ApiModelProperty("化验单id")
    private Integer testId;

    /**
     * 化验单编号
     */
    @ApiModelProperty("化验单编号")
    private String testCode;

    @ApiModelProperty("化验结果")
    private String result;

    /**
     * 水分率
     */
    @ApiModelProperty("水分率")
    private String waterRate;
    /**
     * 实收数量
     */
    @ApiModelProperty("实收数量")
    private BigDecimal num;

    /**
     * 入库日期
     */
    @ApiModelProperty("入库日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;



}
