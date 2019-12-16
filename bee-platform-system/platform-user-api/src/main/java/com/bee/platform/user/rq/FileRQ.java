package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author liliang
 * @since 2019-04-28
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "单个附件rq")
public class FileRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("附件名字")
    private String name;

    @ApiModelProperty("附件url")
    private String url;

}
