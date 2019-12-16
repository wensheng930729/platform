package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 料批主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("料批主单信息请求参数")
public class ErpMaterialBatchOrderRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 料批名称
     */
    @ApiModelProperty("料批名称")
    @NotNull(message = "料批名称不能为空")
    private String materialBatchName;
    /**
     * 产成品id
     */
    @ApiModelProperty("产成品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;
    /**
     * 产成品名称
     */
    @ApiModelProperty("产成品名称")
//    @NotEmpty(message = "产品名称不能为空")
    private String productName;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
//    @NotEmpty(message = "公司名称不能为空")
    private String companyName;
    /**
     * 数量
     */
    @ApiModelProperty("数量")
    @NotNull(message = "数量不能为空")
    @Min(value = 0,message = "数量不能小于0")
    private BigDecimal number;

    @ApiModelProperty("综合入炉品位")
    @NotNull(message = "综合入炉品位不能为空")
    private BigDecimal grade;

    /**
     * 附件名称
     */
    @ApiModelProperty("附件名称")
    private String fileName;
    /**
     * 附件url
     */
    @ApiModelProperty("附件url")
    private String fileUrl;



}
