package com.bee.platform.datadriver.dto;


import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

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
@ApiModel("库存盘点返回信息123")
@JsonInclude
public class ErpStockCheckDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 库存盘点名称
     */
    @ApiModelProperty("库存盘点名称")
    private String stockCheckName;
    /**
     * 库存盘点日期
     */
    @ApiModelProperty("库存盘点日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date stockCheckTime;


    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
    private String companyName;/**
     * 公司名称
     */
    @ApiModelProperty("公司id")
    private Integer companyId;

    /**
     * 确认状态（0已保存，1已确认）
     */
    @ApiModelProperty("确认状态（0已保存，1已确认）")
    private Integer state;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
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

    @ApiModelProperty("库存盘点明细返回信息")
    List<ErpStockCheckDetailDTO> detailDTOList;

}
