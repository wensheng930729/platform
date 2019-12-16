package com.bee.platform.datadriver.rq;

import com.bee.platform.datadriver.dto.ErpLogoDTO;
import com.bee.platform.datadriver.dto.ErpProductCategoryCheckItemsDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author liang.li
 * @ClassName ErpProductUpdateRQ
 * @Description erp产品修改入参
 * @Date 2019-5-29
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("erp产品修改入参")
public class ErpProductUpdateRQ implements Serializable {

    private static final long serialVersionUID = -1L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("产品编码")
    private String code;

    @ApiModelProperty("产品名称")
    private String name;

    @ApiModelProperty("产品单位")
    private String unit;

    @ApiModelProperty("产品类别")
    private Integer category;

    @ApiModelProperty("启用批次，1-启用，0-不启用")
    private Integer enableBatch;

    @ApiModelProperty("状态:1-启用,0-禁用")
    private Integer status;

    @ApiModelProperty("产品logo")
    private List<ErpLogoDTO> logo;

    @ApiModelProperty("产品检测属性")
    private List<ErpProductCategoryCheckItemsDTO> checkItems;

}
