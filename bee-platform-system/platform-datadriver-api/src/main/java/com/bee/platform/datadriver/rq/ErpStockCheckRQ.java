package com.bee.platform.datadriver.rq;


import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 库存盘点主单表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("库存盘点请求参数")
public class ErpStockCheckRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 库存盘点名称
     */
    @ApiModelProperty("库存盘点名称")
    @NotNull(message = "库存盘点名称不能为空")
    private String stockCheckName;
    /**
     * 库存盘点日期
     */
    @ApiModelProperty("库存盘点日期")
    @NotNull(message = "库存盘点日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date stockCheckTime;
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
//    @NotNull(message = "公司名称不能为空")
    private String companyName;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    @Length(max=200,message = "备注不超过200字")
    private String remark;

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
