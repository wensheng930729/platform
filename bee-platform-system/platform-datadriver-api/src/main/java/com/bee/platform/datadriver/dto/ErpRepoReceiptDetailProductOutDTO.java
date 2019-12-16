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
@ApiModel("成品出库根据id查询详情入库明细返回信息")
@JsonInclude
public class ErpRepoReceiptDetailProductOutDTO implements Serializable {

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
     * 出库日期
     */
    @ApiModelProperty("出库日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;

    @ApiModelProperty("车牌号")
    private String plateNo;

    @ApiModelProperty("毛重")
    private BigDecimal roughWeight;

    @ApiModelProperty("皮重")
    private BigDecimal weight;
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

    @ApiModelProperty("出厂品位")
    private BigDecimal grade;

    /**
     * 出库数量
     */
    @ApiModelProperty("出库数量")
    private BigDecimal num;





}
