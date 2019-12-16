package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @Classname RepoReceiptDetailDto
 * @Description 货单明细返回信息
 * @Date 2019/5/29 16:49
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "货单明细返回信息")
public class RepoReceiptDetailDTO implements Serializable {
    private static final long serialVersionUID = 7431811736446596609L;

    @ApiModelProperty("待发货")
    private Integer id;

    @ApiModelProperty("关联的仓库单据id")
    private Integer receiptId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("提单号")
    private String voucherNo;

    @ApiModelProperty("湿重")
    private BigDecimal wetWeight;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("仓库id")
    private Integer repositoryId;

    @ApiModelProperty("化验单id")
    private Integer testId;

    @ApiModelProperty("化验单号")
    private String testCode;

    @ApiModelProperty("料批id")
    private Integer batchId;

    @ApiModelProperty("水分率")
    private String waterRate;

    @ApiModelProperty("数量")
    private BigDecimal num;

    @ApiModelProperty("采购批次号")
    private String purchaseBatch;

    @ApiModelProperty("车牌号")
    private String plateNo;

    @ApiModelProperty("毛重")
    private BigDecimal roughWeight;

    @ApiModelProperty("皮重")
    private BigDecimal weight;

    @ApiModelProperty("品位")
    private BigDecimal grade;

    @ApiModelProperty("仓库名称")
    private String repositoryName;

    @ApiModelProperty("创建人")
    private Integer createUser;

    @ApiModelProperty("记录日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;

    @ApiModelProperty("是否删除：1是 0否")
    private Integer deleted;

    @ApiModelProperty("产品批次名称")
    private String batchName;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("合同重量")
    private BigDecimal orderNum;
}
